import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.nio.channels.ServerSocketChannel;
import java.util.Iterator;

public class TimedReader {

	Selector selector = null;
	ByteBuffer buf;
	public TimedReader(int port){
		try {
			selector = Selector.open();
			// Create a non-blocking socket channel
			ServerSocketChannel sChannel = ServerSocketChannel.open();
			InetSocketAddress listenAddr = new InetSocketAddress("localhost", port);
			sChannel.socket().bind(listenAddr);
			SocketChannel channel = sChannel.accept();
			sChannel.configureBlocking(false);
			channel.configureBlocking(false);
			// register channel with selector for further IO
			channel.register(selector, SelectionKey.OP_READ);
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		buf = ByteBuffer.allocate(1024);
	}

	public String timeoutRead(long timeout){
		try {
			// Wait for an event
			selector.select(timeout);
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}

		Iterator<SelectionKey> keys = this.selector.selectedKeys().iterator();
		while (keys.hasNext()) {
			SelectionKey selKey = keys.next();
			// this is necessary to prevent the same key from coming up 
			// again the next time around.
			keys.remove();
			int numRead = -1;
			if (! selKey.isValid()) {
				continue;
			}
			else if (selKey.isReadable()) {
				SocketChannel channel = (SocketChannel) selKey.channel();
				
				try {
					buf.clear();
					numRead = channel.read(buf);
				}
				catch (IOException e) {
					e.printStackTrace();
				}

				if (numRead == -1) {
					try {
						channel.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
					selKey.cancel();
					return null;
				}
			}
			
			byte[] data = new byte[numRead];
			System.arraycopy(buf.array(), 0, data, 0, numRead);
			try {
				return new String(data, "US-ASCII");
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				return null;
			}
		}
		return "";
	}
}
