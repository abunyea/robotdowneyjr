import java.io.IOException;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;


public class ConnectionThread extends Thread{
	private int port;
	private TimedReader selector;
	private PrintWriter out;
	
	public ConnectionThread(int port){
		this.port = port;
		
	}
	public void run(){
		try { 
			ServerSocket writeSocket = new ServerSocket(port);
			Socket connection = writeSocket.accept();
			out = new PrintWriter(connection.getOutputStream(), true);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		selector = new TimedReader(port+1);		
	}
	
	public TimedReader getSelector(){
		return selector;
	}
	public PrintWriter getPrintWriter(){
		return out;
	}

}
