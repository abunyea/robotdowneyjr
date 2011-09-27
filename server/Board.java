import java.util.HashSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;


public class Board {
    PrintWriter log = new PrintWriter(System.err);
	HashSet<Integer> validMoves = new HashSet<Integer>();
	private int[][] board = new int[17][25];
	int[] specialsLeft = new int[] {2, 2};
	int[] timeLimit = new int[] {10*60*1000, 10*60*1000};
    int turn;
	private static final int homexy[][][] = new int[][][] {
		/* Position 0 */
		{ {16, 12},
			{15, 11}, {15, 13},
			{14, 10}, {14, 12}, {14, 14},
			{13, 9},  {13, 11}, {13, 13}, {13, 15} },
			/* Position 3 */
			{ {0, 12},
				{1, 11}, {1, 13},
				{2, 10}, {2, 12}, {2, 14},
				{3, 9},  {3, 11}, {3, 13}, {3, 15} },
	};

	private static final String initialBoard[] = new String[] {
		"            *            ",
		"           * *           ",
		"          * * *          ",
		"         * * * *         ",
		"* * * * * * * * * * * * *",
		" * * * * * * * * * * * * ",
		"  * * * * * * * * * * *  ",
		"   * * * * * * * * * *   ",
		"    * * * * * * * * *    ",
		"   * * * * * * * * * *   ",
		"  * * * * * * * * * * *  ",
		" * * * * * * * * * * * * ",
		"* * * * * * * * * * * * *",
		"         * * * *         ",
		"          * * *          ",
		"           * *           ",
		"            *            "
	};

	public static int dist(int r1, int c1, int r2,int c2){
		int dr = r2 - r1;
		int ds = (r1+c1-r2-c2)/2;
		if (dr*ds>0){
			return Math.abs(dr)+ Math.abs(ds);
		}
		else{
			return Math.max(Math.abs(dr), Math.abs(ds));
		}
	}

	public static boolean isForward(int i, int j, int r, int c, int myturn) {
		int dir=0;
		if(myturn==1){
			dir = 1;
		}
		else{
			dir = -1;
		}
		return dir*(i-r)>=0;
	}

    public void setLog(PrintWriter pw){
        log = pw;
    }

	public boolean readConfig(Scanner sc) {
		for(int i=0; i<17; i++){
			if(!sc.hasNextLine()) 
				return false;
			String line = sc.nextLine();
			for (int j = 0; j < 25; j++) {
				if(line.charAt(j)== ' ') {
					board[i][j] = -1;
				}
				else {
					board[i][j] = line.charAt(j)-'0';
				}
			}
		}
        if(!sc.hasNextLine()){
            return false;
        }
        String line = sc.nextLine();
        String[] part=line.split(" ");
        if(part.length!=5){
            return false;
        }
        try{
            timeLimit[0]=Integer.parseInt(part[0]);
            timeLimit[1]=Integer.parseInt(part[1]);
            specialsLeft[0]=Integer.parseInt(part[2]);
            specialsLeft[1]=Integer.parseInt(part[3]);
            turn=Integer.parseInt(part[4]);
            if(turn<1 || turn >2)
                return false;
        }catch(Exception e){
            return false;
        }
		return true;
	}

	public boolean checkWin(int myturn) {
		int marbles[] = new int[2];
        //Check opponent's home
		for(int j=0; j<10; j++){
			int x=homexy[2-myturn][j][0];
			int y=homexy[2-myturn][j][1];
			if (board[x][y]!=0){
				marbles[board[x][y]-1]+=1;
			}
		}
        //If all places in the opponent's home are occupied and
        //the marbles of the opponent are less or equal
        //to the marbles of the player then it's gameover.
		if (marbles[0]+marbles[1]==10 && marbles[myturn-1]>=marbles[2-myturn])
			return true;
		else 
			return false;
	}

	public Move validateMove(String s, int myturn){
		String[] coordinates = s.split(" ");
		int status = -1;
		int t1 = -1;
		int t2 = -1;
		int r1 = -1;
		int c1 = -1;
		int r2 = -1;
		int c2 = -1;
		int r3 = -1;
		int c3 = -1;
		if (coordinates.length != 9) {
			log.println("#player sent too many or too few numbers");
			return null;
		} 
		try {
			status = Integer.parseInt(coordinates[0]);
			t1 = Integer.parseInt(coordinates[1]);
			t2 = Integer.parseInt(coordinates[2]);
			r1 = Integer.parseInt(coordinates[3]);
			c1 = Integer.parseInt(coordinates[4]);
			r2 = Integer.parseInt(coordinates[5]);
			c2 = Integer.parseInt(coordinates[6]);
			r3 = Integer.parseInt(coordinates[7]);
			c3 = Integer.parseInt(coordinates[8]);
			if(validateSimpleMove(r1,c1,r2,c2,r3,c3, myturn)){
				return new Move(status, t1, t2, r1, c1, r2, c2, r3, c3);
			}
			else{
				return null;
			}
		} catch (NumberFormatException e) {
			log.println("#player sent a non-number");
			return null;
		}
	}
	
	public Move validateBackwardMove(String s, int myturn) {
		String[] coordinates = s.split(" ");
		int status = -1;
		int t1 = -1;
		int t2 = -1;
		int r1 = -1;
		int c1 = -1;
		int r2 = -1;
		int c2 = -1;
		int r3 = -1;
		int c3 = -1;
		if (coordinates.length != 9) {
			log.println("#player sent too many or too few numbers");
			return null;
		} 
		try {
			status = Integer.parseInt(coordinates[0]);
			t1 = Integer.parseInt(coordinates[1]);
			t2 = Integer.parseInt(coordinates[2]);
			r1 = Integer.parseInt(coordinates[3]);
			c1 = Integer.parseInt(coordinates[4]);
			r2 = Integer.parseInt(coordinates[5]);
			c2 = Integer.parseInt(coordinates[6]);
			r3 = Integer.parseInt(coordinates[7]);
			c3 = Integer.parseInt(coordinates[8]);
			if(validateSimpleBackwardMove(r2,c2,r1,c1,r3,c3, 3-myturn)){
				return new Move(status, t1, t2, r1, c1, r2, c2, r3, c3);
			}
			else{
				return null;
			}
		} catch (NumberFormatException e) {
			log.println("#player sent a non-number");
			return null;
		}
	}

	public boolean validateSimpleMove(int r1,int c1,int r2,int c2,int r3,int c3, int myturn){
		if (!(r1<17 && r1>=0 && c1 >=0 && c1 <25)){
			log.println("#player sent an invalid origin");
            return false;
        }
		if (board[r1][c1] != myturn){
			log.println("#player sent an origin that does not contain its piece");
            return false;
        }
		if (r3!=-1 && c3 !=-1){
			specialsLeft[myturn-1]--;
			if(specialsLeft[myturn-1]<0){
			    log.println("#player used too many special marbles");
				return false;
            }
			if (!(r3<17 && r3>=0 && c3 >=0 && c3 <25)){
			    log.println("#player tried to a place special marble outside the board");
				return false;
            }
			// check home position
			for (int i=0; i<2; i++){
				for(int j=0; j<10; j++){ 
					int x=homexy[i][j][0];
					int y=homexy[i][j][1];
					if (r3 == x && c3 == y){
			            log.println("#player tried to place a special marble inside a home position");
						return false;
                    }
				}
			}
			if (board[r3][c3] != 0){
			    log.println("#player tried to place a special marble in a non-empty location");
				return false;
            }
			//Temporarily add the marble to generate all moves 
			board[r3][c3] = 3;
		}
		legalMoves(r1,c1,validMoves);
		if (!validMoves.contains(25*r2+c2)){
			    log.println("#player gave an invalid destination ");
			return false;
        }
		//everything looks OK restore things if needed
		if (r3!=-1 && c3 !=-1){
			board[r3][c3] = 0;
		}
		return true; 
	}

	private boolean validateSimpleBackwardMove(int r1, int c1, int r2, int c2,
			int r3, int c3, int myturn) {
			if (!(r1<17 && r1>=0 && c1 >=0 && c1 <25)){
				log.println("#player sent an invalid origin");
	            return false;
	        }
			if (board[r1][c1] != myturn){
				log.println("#player sent an origin that does not contain its piece");
	            return false;
	        }
			if (r3!=-1 && c3 !=-1){
				specialsLeft[myturn-1]++;
				if(specialsLeft[myturn-1]>2){
				    log.println("#player used too many special marbles");
					return false;
	            }
				if (!(r3<17 && r3>=0 && c3 >=0 && c3 <25)){
				    log.println("#player tried to take special marble from outside the board");
					return false;
	            }
				if (board[r3][c3] != 3){
				    log.println("#player tried to take a special marble from a location that does not contain it");
					return false;
	            }
			}
			legalMoves(r1,c1,validMoves);
			if (!validMoves.contains(25*r2+c2)){
				    log.println("#player gave an invalid destination ");
				return false;
	        }
			return true; 
	}

	public void legalMoves(int r, int c, HashSet<Integer> moves){
		moves.clear();
		/* Immediate moves */
		int[][] immediate = new int[][] {{r,c-2},{r-1,c-1},{r-1,c+1},{r,c+2},{r+1,c+1},{r+1,c-1}};		
		/* First add the immediate moves */
		for(int i=0; i<immediate.length; i++){
			int r2 = immediate[i][0];
			int c2 = immediate[i][1];
			if(0<=r2 && r2<17 && 0<=c2 && c2<25 && board[r2][c2]==0)
				moves.add(25*r2+c2);
		}
		/* Now add all jumps recursively */
		jump(r,c,moves);
	}

	private void jump(int r, int c, HashSet<Integer> moves){
		/* Jump moves */
		int[][] jumps = new int[][] {{r,c-2,r,c-4},{r-1,c-1,r-2,c-2},{r-1,c+1,r-2,c+2},{r,c+2,r,c+4},{r+1,c+1,r+2,c+2},{r+1,c-1,r+2,c-2}};
		for(int i=0; i<jumps.length; i++){
			int r1=jumps[i][0];
			int c1=jumps[i][1];
			int r2=jumps[i][2];
			int c2=jumps[i][3];
			if(0<=r2 && r2<17 && 0<=c2 && c2<25 && board[r1][c1]>0 && board[r2][c2]==0 && !moves.contains(25*r2+c2)){
				moves.add(25*r2+c2);
				jump(r2,c2,moves);
			}

		}
	}

	public void move(Move m) {
		if (m.r3!=-1 && m.c3 !=-1){
			board[m.r3][m.c3] = 3;
		}
		board[m.r2][m.c2] = board[m.r1][m.c1];
		board[m.r1][m.c1] = 0;		
        turn = 3-turn;
	}

	public void backwardMove(Move m) {
		if (m.r3!=-1 && m.c3 !=-1){
			board[m.r3][m.c3] = 0;
		}
		board[m.r1][m.c1] = board[m.r2][m.c2];
		board[m.r2][m.c2] = 0;		
        turn = 3-turn;
	}

	public int at(int i, int j) {
		return board[i][j];
	}

	public void setSpecialMarble(int myturn, int i, int j) {
		board[i][j]=3;
		specialsLeft[myturn-1]--;
	}

	public boolean canSetSpecialMarble(int myturn) {
		return (specialsLeft[myturn-1]>0);
	}

    public Board(String s){
        if(s==null){
            create();
            return;
        }
        try{
            Scanner sc = new Scanner(new File(s));
            if(!readConfig(sc))
                create();
        }catch(Exception e){
            create();
            return;
        }
    }

	public void create() {
		//board = new int[17][25];
		for (int i = 0; i < 17; i++) {
			for (int j = 0; j < 25; j++) {
				if (initialBoard[i].charAt(j) == '*') {
					board[i][j] = 0;
				} else {
					board[i][j] = -1;
				}
			}
		}

		for (int player = 0; player < 2; player++) {
			for (int j = 0; j < 10; j++) {
				int x = homexy[player][j][0];
				int y = homexy[player][j][1];
				board[x][y] = player + 1;
			}
		}
	    specialsLeft[0]=2;
        specialsLeft[1]=2;
	    timeLimit[0]=10*60*1000;
        timeLimit[1]=10*60*1000;
        turn=1;
	}

    public int getSpecials(int i){
        return specialsLeft[i-1];
    }

    public int getTimeLimit(int i){
        return timeLimit[i-1];
    }

    public int getTurn(){
        return turn;
    }

	public String toString(char zero){
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < 17; i++) {
			for (int j = 0; j < 25; j++) {
				if (board[i][j] == -1)
					sb.append(" ");
				else if (board[i][j] == 0)
					sb.append(zero);
				else
					sb.append(board[i][j]);
			}
			sb.append("\n");
		}
        sb.append(timeLimit[0]+" "+timeLimit[1]+" "+specialsLeft[0]+" "+specialsLeft[1]+" "+turn+"\n");
		return sb.toString();
	}

	public String toString2(char zero){
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < 17; i++) {
			sb.append("#");
			for (int j = 0; j < 25; j++) {
				if (board[i][j] == -1)
					sb.append(" ");
				else if (board[i][j] == 0)
					sb.append(zero);
				else
					sb.append(board[i][j]);
			}
			sb.append("\n");
		}
        sb.append("#"+timeLimit[0]+" "+timeLimit[1]+" "+specialsLeft[0]+" "+specialsLeft[1]+" "+turn+"\n");
		return sb.toString();
	}

}
