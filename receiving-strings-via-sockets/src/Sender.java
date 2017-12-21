/**
 * Java server that sends a bunch of strings.
 */

import java.net.*;
import java.io.*;

public class Sender {
    public static void main(String[] args) throws IOException, InterruptedException {
        ServerSocket serverSock = new ServerSocket(9090);

        System.out.println("Accepting connections...");

        Socket sock = serverSock.accept();

        OutputStream outStream = sock.getOutputStream();
        PrintWriter sockOut = new PrintWriter(new OutputStreamWriter(outStream));

        while (true) {
            Thread.sleep(1000);
            System.out.println("Sending foo");
            sockOut.println("foo");
            sockOut.flush();
            
            Thread.sleep(1000);
            System.out.println("Sending abracadabra");
            sockOut.println("abracadabra");
            sockOut.flush();
            
            Thread.sleep(1000);
            System.out.println("Sending an empty string");
            sockOut.println("");
            sockOut.flush();
            
            Thread.sleep(1000);
            System.out.println("Sending very long very long very long");
            sockOut.println("very long very long very long");
            sockOut.flush();
        }
    }
}
