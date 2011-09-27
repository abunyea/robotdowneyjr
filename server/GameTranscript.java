import java.util.ArrayList;
import java.util.List;


public class GameTranscript {
	
	List<String> moves;
	int idx;
	
	public GameTranscript() {
		moves = new ArrayList<String>();
		idx = 0;
	}

	public void add(String m) {
		moves.add(m);
	}

	public String previous() {
		if (idx>0)
			return moves.get(--idx);
		else 
			return null;
	}

	public String next() {
		if(idx<moves.size()){
			return moves.get(idx++);
		}
		else
			return null;
	}

}
