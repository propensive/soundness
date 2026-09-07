package burdock;

import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.lang.reflect.*;
import java.security.*;
import java.util.jar.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

// The runtime entry point of a repackaged JAR. The repackager force-includes exactly ONE class
// file, `burdock/Bootstrap.class`, so everything here must compile into it: no nested or
// anonymous classes and no records (lambdas are fine; they need no class file of their own).
public class Bootstrap {
  static int verbosity = 1;

  // Concurrent downloads. Bounded, because every requirement of a typical application
  // lives on one or two hosts, and a hundred simultaneous connections to a release server
  // invites throttling rather than throughput.
  static final int PARALLELISM = 8;
  static final int CONNECT_TIMEOUT_MS = 10_000;
  static final int READ_TIMEOUT_MS = 30_000;
  static final int BUFFER_SIZE = 65_536;

  // Startup progress, reported to whatever launched this JVM through the file named by the
  // `burdock.progress` system property (Ethereal's launcher sets it to a path in the daemon's
  // state directory, and extends its startup deadline while the file keeps changing). With no
  // property set, reporting is a no-op. The file holds one line, `<completed> <total> <bytes>`:
  // requirements verified so far, requirements in total, and bytes downloaded so far. Writes go
  // through a temporary sibling and an atomic rename so a reader never sees a partial line, and
  // are throttled so a fast download does not turn into thousands of renames.
  static final long PROGRESS_INTERVAL_MS = 200;
  static Path progressPath = null;
  static Path progressTemp = null;
  static int progressTotal = 0;
  static final AtomicInteger progressCompleted = new AtomicInteger();
  static final AtomicLong progressBytes = new AtomicLong();
  static long progressLastWrite = 0;

  // Also the exit path from a worker thread, so the `finally` that would normally remove the
  // progress file never runs: remove it here, or a later launch would read a stale position.
  static void quit(String message, int status) {
    if (verbosity != 0) System.err.println(message);
    progressFinish();
    System.exit(status);
  }

  static void info(String message, int severity) {
    if (verbosity >= severity) System.err.println(message);
  }

  // Each `Burdock-Require` item is `<64 hex chars>:<url>`, as written by the repackager. Parsed
  // into `{hash, url}` pairs.
  static List<String[]> parseRequirements(String requirements) {
    List<String[]> result = new ArrayList<>();
    if (requirements == null) return result;
    for (String item : requirements.split(" ")) {
      if (item.isEmpty()) continue;
      if (item.length() < 66 || item.charAt(64) != ':')
        quit("The Burdock-Require entry \""+item+"\" is malformed.", 2);
      result.add(new String[] { item.substring(0, 64), item.substring(65) });
    }
    return result;
  }

  static File cacheDir() {
    String cacheEnv = System.getenv("XDG_CACHE_HOME");
    File cache = (cacheEnv == null || cacheEnv.isEmpty())
        ? new File(new File(System.getProperty("user.home")), ".cache")
        : new File(cacheEnv);
    return new File(cache, "burdock");
  }

  static String hex(byte[] bytes) {
    StringBuilder builder = new StringBuilder();
    for (byte b : bytes) builder.append(String.format("%02x", b));
    return builder.toString();
  }

  static void progressStart(int total) {
    String property = System.getProperty("burdock.progress");
    if (property == null || property.isEmpty()) return;
    progressPath = Paths.get(property);
    progressTemp = progressPath.resolveSibling(progressPath.getFileName()+".tmp");
    progressTotal = total;
    progressReport(true);
  }

  static void progressDownloaded(int count) {
    progressBytes.addAndGet(count);
    progressReport(false);
  }

  static void progressCompleted() {
    progressCompleted.incrementAndGet();
    progressReport(true);
  }

  static synchronized void progressReport(boolean force) {
    if (progressPath == null) return;
    long now = System.currentTimeMillis();
    if (!force && now - progressLastWrite < PROGRESS_INTERVAL_MS) return;
    progressLastWrite = now;
    String line = progressCompleted.get()+" "+progressTotal+" "+progressBytes.get()+"\n";
    try {
      Files.writeString(progressTemp, line);
      try {
        Files.move(progressTemp, progressPath, StandardCopyOption.ATOMIC_MOVE,
            StandardCopyOption.REPLACE_EXISTING);
      } catch (AtomicMoveNotSupportedException e) {
        Files.move(progressTemp, progressPath, StandardCopyOption.REPLACE_EXISTING);
      }
    } catch (IOException e) {
      // Progress is advisory: a launcher that cannot read it just falls back to its fixed
      // deadline, so a failure to write it must not fail the application.
    }
  }

  static synchronized void progressFinish() {
    if (progressPath == null) return;
    try { Files.deleteIfExists(progressPath); } catch (IOException e) {}
    try { Files.deleteIfExists(progressTemp); } catch (IOException e) {}
  }

  // Returns the cached, verified JAR for one requirement, downloading it if necessary. Runs on
  // a worker thread; a checksum mismatch quits the whole process, as it always has.
  static File fetch(String requiredHash, String location, File dir) throws Exception {
    URL url = new URI(location).toURL();
    File file = new File(dir, requiredHash+".jar");
    info("Application requires "+url+".", 3);

    if (file.exists()) {
      info("JAR file exists locally at "+file+".", 3);
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      try (InputStream in = new FileInputStream(file)) {
        byte[] buffer = new byte[BUFFER_SIZE];
        int n;
        while ((n = in.read(buffer)) != -1) digest.update(buffer, 0, n);
      }
      String calculatedHash = hex(digest.digest());
      if (!calculatedHash.equals(requiredHash))
        quit("SHA-256 checksum of local dependency "+file+" does not match hash value ("+
            requiredHash+").", 1);
    } else {
      info("File does not exist locally.", 3);
      info("Downloading "+url, 2);
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      File temp = File.createTempFile("tempfiles", ".tmp", dir);
      boolean moved = false;

      try {
        URLConnection connection = url.openConnection();
        connection.setConnectTimeout(CONNECT_TIMEOUT_MS);
        connection.setReadTimeout(READ_TIMEOUT_MS);

        try (InputStream in = connection.getInputStream();
             OutputStream out = new FileOutputStream(temp)) {
          byte[] buffer = new byte[BUFFER_SIZE];
          int n;
          while ((n = in.read(buffer)) != -1) {
            out.write(buffer, 0, n);
            digest.update(buffer, 0, n);
            progressDownloaded(n);
          }
        }

        String calculatedHash = hex(digest.digest());
        info("Calculated hash of downloaded file as "+calculatedHash+".", 3);

        if (!calculatedHash.equals(requiredHash)) {
          temp.delete();
          quit("SHA-256 checksum of dependency "+url+" does not match expected value ("+
              requiredHash+").", 1);
        }

        // Another process (a retried launch, say) may have completed the same download
        // meanwhile; both wrote identical, verified bytes, so replacing is harmless.
        try {
          Files.move(temp.toPath(), file.toPath(), StandardCopyOption.ATOMIC_MOVE,
              StandardCopyOption.REPLACE_EXISTING);
        } catch (AtomicMoveNotSupportedException e) {
          Files.move(temp.toPath(), file.toPath(), StandardCopyOption.REPLACE_EXISTING);
        }
        moved = true;
      } finally {
        if (!moved) temp.delete();
      }
    }

    progressCompleted();
    return file;
  }

  // Fetches every requirement, in parallel, returning their files in manifest order so the
  // classpath order is the one the repackager wrote.
  static List<File> fetchAll(List<String[]> requirements) throws Exception {
    List<File> files = new ArrayList<>();
    if (requirements.isEmpty()) return files;
    File dir = cacheDir();
    dir.mkdirs();
    progressStart(requirements.size());
    ExecutorService executor =
        Executors.newFixedThreadPool(Math.min(requirements.size(), PARALLELISM));

    try {
      List<Future<File>> futures = new ArrayList<>();
      for (String[] requirement : requirements)
        futures.add(executor.submit(() -> fetch(requirement[0], requirement[1], dir)));

      for (Future<File> future : futures) {
        try { files.add(future.get()); }
        catch (ExecutionException e) {
          Throwable cause = e.getCause();
          throw (cause instanceof Exception exception) ? exception : e;
        }
      }
    } finally {
      executor.shutdownNow();
      progressFinish();
    }

    return files;
  }

  public static void main(String[] args) throws Exception {
    Attributes attributes = null;
    List<URL> jars = new ArrayList<>();
    String mainClassname = null;

    try (InputStream manifest =
        Bootstrap.class.getClassLoader().getResourceAsStream("META-INF/MANIFEST.MF")) {

      if (manifest != null) attributes = new Manifest(manifest).getMainAttributes();
      else quit("Manifest file not found!", 2);

      CodeSource codeSource = Bootstrap.class.getProtectionDomain().getCodeSource();
      if (codeSource != null) jars.add(codeSource.getLocation().toURI().toURL());

      String verbosityLevel = attributes.getValue("Burdock-Verbosity");
      mainClassname = attributes.getValue("Burdock-Main");

      verbosity = switch (verbosityLevel) {
        case "silent" -> 0;
        case "error"  -> 1;
        case null     -> 1;
        case "info"   -> 2;
        case "debug"  -> 3;
        default       -> -1;
      };

      if (verbosity < 0) quit("invalid verbosity level: "+verbosityLevel, 2);

      for (File file : fetchAll(parseRequirements(attributes.getValue("Burdock-Require"))))
        jars.add(file.toURI().toURL());

      if (mainClassname == null) quit("The main method has not been specified.", 2);

      URL[] jarfiles = jars.toArray(new URL[0]);

      try (URLClassLoader classLoader = new URLClassLoader(jarfiles,
          Bootstrap.class.getClassLoader().getParent())) {
        Thread.currentThread().setContextClassLoader(classLoader);
        Class<?> mainClass = classLoader.loadClass(mainClassname);
        Method mainMethod = mainClass.getMethod("main", String[].class);
        mainMethod.invoke(null, (Object) args);
      }
    } catch (Throwable exception) {
      System.err.println("Failed to launch the application");
      System.err.println(exception);
      exception.printStackTrace(System.err);
      System.exit(2);
    }
  }
}
