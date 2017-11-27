/**
 * Client to test the 'hClose' behavior with.
 */

import java.net.*;
import java.io.*;

public class Reader {
    public static void main(String[] args) throws IOException, InterruptedException {

        ServerSocket serverSock = new ServerSocket(9090);

        Socket sock = serverSock.accept();

        InputStream inStream = sock.getInputStream();
        BufferedReader sockIn = new BufferedReader(new InputStreamReader(inStream));

        while (true) {
            Thread.sleep(1000);
            System.out.println("Trying to read something...");
            String s = sockIn.readLine();
            System.out.println("Got " + s );
        }
    }
}
