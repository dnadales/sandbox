/**
 * Client to test the 'hClose' behavior with.
 */

import java.net.*;
import java.io.*;

public class Client {
    public static void main(String[] args) throws IOException, InterruptedException {

        Socket sock = new Socket("localhost", 9090);

        OutputStream outStream = sock.getOutputStream();

        PrintWriter sockOut = new PrintWriter(new OutputStreamWriter(outStream));

        while (true) {
            Thread.sleep(1000);
            System.out.println("Sending foo");
            sockOut.println("foo");
            sockOut.flush();
            Thread.sleep(1000);
            System.out.println("Sending foo");
            sockOut.println("foo");
            sockOut.flush();
            Thread.sleep(1000);
            System.out.println("Sending quit");
            sockOut.println("quit");
            sockOut.flush();
        }
    }
}
