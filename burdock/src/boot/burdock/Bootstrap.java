package burdock;

import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.lang.reflect.*;
import java.security.*;
import java.util.jar.*;
import java.util.*;

public class Bootstrap {
  static int verbosity = 1;

  static void quit(String message, int status) {
    if (verbosity != 0) System.err.println(message);
    System.exit(status);
  }

  static void info(String message, int severity) {
    if (verbosity >= severity) System.err.println(message);
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
      String cacheEnv = System.getenv("XDG_CACHE_HOME");
      File cache = null;

      if (cacheEnv == null || cacheEnv.isEmpty())
        cache = new File(new File(System.getProperty("user.home")), ".cache");

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

      String requirements = attributes.getValue("Burdock-Require");
      if (requirements != null) {
        for (String item : requirements.split(" ")) {
          String requiredHash = item.substring(0, 64);
          URL url = new URI(item.substring(65)).toURL();
          info("Application requires "+url+".", 3);
          File dir = new File(cache, "burdock");
          File temp = dir.createTempFile("tempfiles", ".tmp");
          dir.mkdirs();
          File file = new File(dir, requiredHash+".jar");

          if (!file.exists()) {
            info("File does not exist locally.", 3);
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            String calculatedHash = null;

            info("Downloading "+url, 2);

            try (InputStream in = url.openStream();
                 FileOutputStream out = new FileOutputStream(temp)) {

              byte[] buffer = new byte[4096];
              int n = 0;

              while ((n = in.read(buffer)) != -1) {
                out.write(buffer, 0, n);
                digest.update(buffer, 0, n);
              }

              byte[] hashBytes = digest.digest();
              StringBuilder builder = new StringBuilder();
              for (byte b : hashBytes) builder.append(String.format("%02x", b));

              calculatedHash = builder.toString();
              out.close();
            }

            info("Calculated hash of downloaded file as "+calculatedHash+".", 3);

            if (calculatedHash.equals(requiredHash))
              Files.move(temp.toPath(), file.toPath(), StandardCopyOption.REPLACE_EXISTING);
            else {
              Files.delete(temp.toPath());
              quit("SHA-256 checksum of dependency "+url+" does not match expected value ("+
                  requiredHash+").", 1);
            }
          } else if (verbosity >= 1) {
            info("JAR file exists locally at "+file+".", 3);
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            try (FileInputStream fis = new FileInputStream(file)) {
              byte[] byteArray = new byte[4096];
              int bytesRead;
              while ((bytesRead = fis.read(byteArray)) != -1) {
                digest.update(byteArray, 0, bytesRead);
              }
              byte[] hashBytes = digest.digest();
              StringBuilder builder = new StringBuilder();
              for (byte b : hashBytes) builder.append(String.format("%02x", b));
              String calculatedHash = builder.toString();

              if (!calculatedHash.equals(requiredHash)) {
                quit("SHA-256 checksum of local dependency "+file+" does not match hash value ("+
                    requiredHash+").", 1);
              }
            }
          }

          jars.add(file.toURI().toURL());
        }
      }

      if (mainClassname == null) quit("The main method has not been specified.", 2);

      URL[] jarfiles = new URL[jars.size()];

      for (int i = 0; i < jarfiles.length; i++) jarfiles[i] = jars.get(i);

      try (URLClassLoader classLoader = new URLClassLoader(jarfiles,
          Bootstrap.class.getClassLoader().getParent())) {
        Thread.currentThread().setContextClassLoader(classLoader);
        Class<?> mainClass = classLoader.loadClass(mainClassname);
        Method mainMethod = mainClass.getMethod("main", String[].class);
        mainMethod.invoke(null, (Object) args);
      }
    } catch (Exception exception) {
      System.err.println(exception);
      System.exit(2);
    }
  }
}
