/**
 * Client to test the 'hClose' behavior with.
 */

import java.net.*;
import java.io.*;

public class Cheater {
    public static void main(String[] args) throws IOException, InterruptedException {

        ServerSocket serverSock = new ServerSocket(9090);

        Socket sock = serverSock.accept();

        while (true) {
            Thread.sleep(1000);
            System.out.println("I'm not reading anything...");
        }
    }
}
