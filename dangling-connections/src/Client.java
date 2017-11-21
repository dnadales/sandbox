/**
 * Client to test the 'hClose' behavior with.
 */

import java.net.*;
import java.io.*;

public class Client {
    public static void main(String[] args) throws IOException, InterruptedException {

        ServerSocket serverSock = new ServerSocket(9090);

        Socket sock = serverSock.accept();

        InputStream inStream = sock.getInputStream();
        BufferedReader sockIn = new BufferedReader(new InputStreamReader(inStream));

        OutputStream outStream = sock.getOutputStream();
        PrintWriter sockOut = new PrintWriter(new OutputStreamWriter(outStream));



        while (true) {
            Thread.sleep(1000);
            System.out.println("Sending foo");
            sockOut.println("foo");
            sockOut.flush();
            String s = sockIn.readLine();
            System.out.println("Got " + s );
            Thread.sleep(1000);
            System.out.println("Sending bar");
            sockOut.println("bar");
            sockOut.flush();
            s = sockIn.readLine();
            System.out.println("Got " + s );
            Thread.sleep(1000);
            System.out.println("Sending quit");
            sockOut.println("quit");
            sockOut.flush();
        }
    }
}
