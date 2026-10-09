import java.io.*;
import java.nio.file.*;
import java.util.*;

// Captures what bash, zsh, fish and PowerShell write to a terminal while a short script is typed at
// them, over guillotine's pseudo-terminal, as fixtures for yossarian's tests. Run it through
// `etc/capture-shell-transcripts.sh`, which supplies the classpath and derives the expected screens.
//
// Each shell runs with a prompt of `> ` set from a configuration file under a fixed scratch home, so
// that nothing of the user's own configuration, name or paths reaches the fixtures. The queries
// shells make of their terminal are answered with fixed replies, so none waits for an answer; the
// cursor position is always reported as the top-left corner, which is why PowerShell, whose line
// editor positions everything absolutely, draws each line at the top of the screen.
public class CaptureShellTranscripts {
  record Step(String keys, int waitMillis) {}

  static final String ENTER = "\r", BS = "\u007f", LEFT = "\u001b[D", TAB = "\t", INTERRUPT = "\u0003";

  static List<Step> script() {
    return List.of(
      new Step("clear" + ENTER, 800),
      new Step("echo hello" + ENTER, 600),
      new Step("echo helXX" + BS + BS + "lo" + ENTER, 600),
      new Step("echo helo" + LEFT + "l" + ENTER, 600),
      new Step("ls /usr/l", 300),
      new Step(TAB, 600),
      new Step(TAB, 800),
      new Step(INTERRUPT, 600),
      new Step("exit" + ENTER, 1000));
  }

  public static void main(String[] args) throws Exception {
    Path out = Path.of(args[0]);
    Files.createDirectories(out);
    Path home = Path.of("/tmp/ptyfixture");
    Files.createDirectories(home.resolve("zsh"));
    Files.createDirectories(home.resolve("config/fish"));
    Files.writeString(home.resolve("bashrc"), "PS1='> '\nbind 'set show-all-if-ambiguous on'\n");
    Files.writeString(home.resolve("zsh/.zshrc"),
      "PROMPT='> '\nRPROMPT=''\nautoload -Uz compinit\ncompinit -u -d /tmp/ptyfixture/zcompdump\n");
    Files.writeString(home.resolve("config/fish/config.fish"),
      "set -g fish_greeting ''\nfunction fish_prompt; echo -n '> '; end\nfunction fish_right_prompt; end\n");
    capture(out, home, "bash", Map.of(), "bash", "--rcfile", home.resolve("bashrc").toString(), "-i");
    capture(out, home, "zsh", Map.of("ZDOTDIR", home.resolve("zsh").toString()), "zsh", "-i");
    capture(out, home, "fish", Map.of("XDG_CONFIG_HOME", home.resolve("config").toString()), "fish", "-i");
    capture(out, home, "pwsh", Map.of(), "pwsh", "-NoProfile", "-NoLogo", "-NoExit", "-Command", "function prompt { '> ' }");
  }

  static void capture(Path out, Path home, String name, Map<String, String> extra, String... command) throws Exception {
    List<Step> steps = script();
    ProcessBuilder builder = new ProcessBuilder(command).directory(home.toFile());
    Map<String, String> env = builder.environment();
    env.clear();
    env.put("TERM", "xterm-256color");
    env.put("HOME", home.toString());
    env.put("PATH", "/opt/homebrew/bin:/usr/bin:/bin");
    env.put("LANG", "en_US.UTF-8");
    env.put("USER", "user");
    env.putAll(extra);
    Process process = guillotine.PtyProcess$.MODULE$.apply(builder, 80, 24);
    InputStream input = process.getInputStream();
    OutputStream output = process.getOutputStream();
    ByteArrayOutputStream transcript = new ByteArrayOutputStream();
    StringBuilder log = new StringBuilder();

    Thread reader = new Thread(() -> {
      byte[] buffer = new byte[8192];
      StringBuilder seen = new StringBuilder();
      int scanned = 0;
      try {
        for (int count; (count = input.read(buffer)) >= 0; ) {
          synchronized (transcript) { transcript.write(buffer, 0, count); }
          seen.append(new String(buffer, 0, count, "ISO-8859-1"));
          for (String[] query : QUERIES) {
            for (int at = seen.indexOf(query[0], scanned); at >= 0; at = seen.indexOf(query[0], at + 1)) {
              synchronized (log) { log.append("reply " + query[1].replace("\u001b", "ESC") + "\n"); }
              output.write(query[1].getBytes("ISO-8859-1"));
              output.flush();
            }
          }
          scanned = Math.max(0, seen.length() - 8);
          for (String[] query : QUERIES) {
            int partial = seen.lastIndexOf(query[0]);
            if (partial >= scanned) scanned = partial + 1;
          }
        }
      } catch (IOException error) {}
    });
    reader.setDaemon(true);
    reader.start();

    Thread.sleep(2000);
    for (Step step : steps) {
      try {
        output.write(step.keys().getBytes("UTF-8"));
        output.flush();
      } catch (IOException error) {
        synchronized (log) { log.append("closed before: " + step.keys() + "\n"); }
        break;
      }
      synchronized (log) { log.append("sent " + step.keys().replace("\u001b", "ESC").replace("\r", "\\r") + "\n"); }
      Thread.sleep(step.waitMillis());
    }
    ((guillotine.PtyProcess) process).close();
    reader.join(2000);
    Files.write(out.resolve(name + ".transcript"), transcript.toByteArray());
    Files.writeString(out.resolve(name + ".log"), log.toString());
    System.out.println(name + ": " + transcript.size() + " bytes, exit " + process.waitFor());
  }

  // Canned answers to the queries shells make of their terminal: the cursor position, and the
  // primary and secondary device attributes. Anything else asked goes unanswered, which a shell
  // must tolerate, since most terminals do not answer it either.
  static final String[][] QUERIES = {
    {"\u001b[6n", "\u001b[1;1R"},
    {"\u001b[c", "\u001b[?62;22c"},
    {"\u001b[0c", "\u001b[?62;22c"},
    {"\u001b[>c", "\u001b[>0;0;0c"},
    {"\u001b[>0c", "\u001b[>0;0;0c"}};
}
