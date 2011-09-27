
public class Move {
	int status; //Does this move win the game  
	int t1; // time for player 1
	int t2; // time for player 2
	int r1; // origin row 
	int c1; // origin column
	int r2; // destination row
	int c2; // destination column
	int r3; // special row
	int c3; // special column
	
	
	public Move(int s, int t1, int t2, int r1, int c1, int r2, int c2, int r3, int c3) {
		this.status = s;
		this.t1 = t1;
		this.t2 = t2;
		this.r1 = r1;
		this.c1 = c1;
		this.r2 = r2;
		this.c2 = c2;
		this.r3 = r3;
		this.c3 = c3;
	}


	public int getRowOrigin() {
		return r1;
	}

	public int getColumnOrigin() {
		return c1;
	}

	public int getRowDestination() {
		return r2;
	}

	public int getColumnDestination() {
		return c2;
	}
	
}