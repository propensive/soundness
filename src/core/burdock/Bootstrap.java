package burdock;

import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.lang.reflect.*;
import java.security.*;
import java.util.*;

public class Bootstrap {
  public static void main(String[] args) throws Exception {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(
        Bootstrap.class.getClassLoader().getResourceAsStream("META-INF/bootstrap.codl")))) {

      String line;
      List<URL> jars = new ArrayList<>();
      CodeSource codeSource = Bootstrap.class.getProtectionDomain().getCodeSource();
      if (codeSource != null) jars.add(codeSource.getLocation().toURI().toURL());
      String mainClassname = null;
      String cacheEnv = System.getenv("XDG_CACHE_HOME");
      File cache = null;
      int verbosity = 1;

      if (cacheEnv == null || cacheEnv.isEmpty())
        cache = new File(new File(System.getProperty("user.home")), ".cache");

      while ((line = reader.readLine()) != null) {
        String[] words = line.split("  *");
        String keyword = words[0];

        if (keyword.equals("verbosity")) {
          String param = words[1];
          if (param.equals("debug")) verbosity = 3;
          else if (param.equals("info")) verbosity = 2;
          else if (param.equals("error")) verbosity = 1;
          else if (param.equals("silent")) verbosity = 0;
          else System.err.println("Invalid verbosity level. Valid values are {debug, info, error}");

        } else if (keyword.equals("require")) {

          URL url = new URI(words[1]).toURL();
          String requiredHash = words[2];
          if (verbosity >= 3) System.err.println("Application requires "+url+".");
          File dir = new File(cache, "burdock");
          File temp = dir.createTempFile("tempfiles", ".tmp");
          dir.mkdirs();
          File destination = new File(dir, requiredHash+".jar");

          if (!destination.exists()) {
            if (verbosity >= 3) System.err.println("File does not exist locally.");
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            String calculatedHash = null;

            if (verbosity >= 2) System.err.println("Downloading "+url+".");

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

            if (verbosity >= 3)
              System.err.println("Calculated hash of downloaded file as "+calculatedHash+".");

            if (calculatedHash.equals(requiredHash))
              Files.move(temp.toPath(), destination.toPath(), StandardCopyOption.REPLACE_EXISTING);
            else {
              if (verbosity >= 1) System.err.println("SHA-256 checksum of dependency "+url+
                  " does not match expected value ("+requiredHash+").");

              Files.delete(temp.toPath());
              System.exit(1);
            }
          } else if (verbosity >= 1) {
            if (verbosity >= 3) System.err.println("JAR file exists locally at "+destination+".");
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            try (FileInputStream fis = new FileInputStream(destination)) {
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
                if (verbosity >= 1) {
                  System.err.println("SHA-256 checksum of local dependency "+
                      destination+" does not match hash value ("+requiredHash+").");
                  System.exit(1);
                }
              }
            }
          }

          jars.add(destination.toURI().toURL());

        } else if (keyword.equals("main")) mainClassname = words[1];
        else {
          System.err.println("The keyword "+keyword+" is not recognized.");
          System.exit(1);
        }
      }

      if (mainClassname == null) {
        if (verbosity >= 1) System.err.println("The main method has not been specified.");
        System.exit(1);
      }

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
      System.exit(2);
    }
  }
}
