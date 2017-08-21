public class SayHi {

    public static void main(String[] args) {
        while (true) {
            System.out.println("Still alive");
            try {Thread.sleep(1000);} catch (Exception ex) {ex.printStackTrace();}            
        }
    }
}
