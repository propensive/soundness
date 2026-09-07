package burdock;

// The application the bootstrap end-to-end tests launch: its class bytes are copied from the
// test classpath into a JAR whose `Burdock-Main` names it. Plain Java, so the JAR needs no
// Scala runtime.
public class Probe {
  public static void main(String[] args) {
    System.out.println("probe");
  }
}
