/**
 * Small experiment to confirm that, unlike Haskell, Java waits for all the
 * threads to terminate before exiting the main program. Which in turns
 * confirms that Haskell's approach to concurrency is more general, and that
 * Haskell is great, and that I will avoid working for any company that uses
 * Java to the extent possible.
 */
public class Spawner implements Runnable {
  private int id;

  Spawner(int id) {
    this.id = id;
  }

  public void run() {
    while (true) {
      try {Thread.sleep(1000);} catch (Exception ex) {ex.printStackTrace();};
      String msg =
        String.format("Thread %d says: Stayin alive, stayin alive, ah-ah-ah-ah!", id);
      System.out.println(msg);
    }
  }
    
  public static void main(String[] args) {
    for (int i = 0; i < 3; i ++) {
      (new Thread(new Spawner(i))).start();
    }
    System.out.println("Bye bye");
  }
}
