package util.sys;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;
import java.util.concurrent.BlockingQueue;

public class Logger implements Runnable {
    private final BlockingQueue<String> messages;
    private final FileWriter fw;
    public Logger(BlockingQueue<String> messages, String fp) throws IOException {
        this.messages = messages;
        fw = new FileWriter(new File(fp));
    }
    public void run() {
        while (true) {
            try {
                    writeMessage(messages.take());
            } catch (InterruptedException e) {
                    System.err.println("Logger has been interrupted. It has "+messages.size()+ " messages remaining at this time. Attempting to write.");
                    while (!messages.isEmpty()) {
                            writeMessage(messages.poll());
                    }
                    try { fw.close();
                    } catch (IOException e1) { }
            }
        }
    }
    private void writeMessage(String s) {
        try {
                String date = getFormattedDate();
                fw.write(date+"::"+s+"\n");
                fw.flush();
        } catch (IOException e) {
                e.printStackTrace();
        }
    }
    private static String getFormattedDate() {
        String[] parts = new Date().toString().split(" ");
        String date = parts[0]+"-"+parts[3];
        return date;
    }
}
