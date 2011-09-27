import java.util.HashSet;
import java.util.Scanner;

public class GreedyAI extends Player {
	
	
	public GreedyAI(Scanner scanner) {
		super(scanner);
	}


	
	public static Move greedy(Board board,int turn){
		int targetR = -1;
		int targetC = -1;
		if (turn==1){
			targetR = 0;
			targetC = 12;
		}else{
			targetR = 16;
			targetC = 12;
		}
		int maxDist = -1;
		int retr1=-1,retr2=-1,retc1=-1,retc2=-1;
		
		HashSet<Integer> hs = new HashSet<Integer>();
		// determine whether to add special marble or not. 
		int retr3 = -1;
		int retc3 = -1;
		if (board.canSetSpecialMarble(turn)) {
			double num = Math.random();
			if (num < 0.4) {
				while (retr3 == -1 && retc3 == -1) {
					int i = (int) (Math.random() * 8 + 4);
					int j = (int) (Math.random() * 8 + 8);
					if (board.at(i,j) == 0) {
						retr3 = i;
						retc3 = j;
						board.setSpecialMarble(turn,retr3,retc3);
					}
				}
			}
		}
		//Find all my marbles		
		for(int i=0; i<17; i++){
			int k=1;
			for (int j = 0; j < 25; j++) {
				if(board.at(i,j) == turn){
					board.legalMoves(i,j,hs);
					for (int dest: hs){
						int r = dest/25;
						int c = dest%25;
						int curDist = Board.dist(i,j,targetR,targetC)- Board.dist(r,c,targetR,targetC);
						if(maxDist< curDist){
							maxDist= curDist;
							retr1 = i;
							retc1 = j;
							retr2 = r;
							retc2 = c;
							k=2;
						}
						else if(maxDist == curDist){
							//Knuth's trick for picking a random element from a stream
							if(k*Math.random() < 1){
								retr1 = i;
								retc1 = j;
								retr2 = r;
								retc2 = c;
							}
							k+=1;
						}
					}
				}
			}
		}
		return new Move(0, 0, 0, retr1,retc1, retr2,retc2, retr3,retc3);
	}

	@Override
	public String think() {
		Move m = greedy(getBoard(),getMyturn());
        //perform the move before sending it
		board.move(m);
		return m.r1+" "+m.c1+" "+m.r2+" "+m.c2 + " "+ m.r3+" "+ m.c3;
	}
	
	public static void main(String args[]){
		int turn = 1;
		GreedyAI p = new GreedyAI(new Scanner(System.in));
		while (true)
		{
			System.err.println("turn = "+turn+"   myturn = "+p.getMyturn());	
			if (turn == p.getMyturn()){
				System.err.println("It is my turn and I am thinking");	
				System.out.println(p.think());
				int status = p.getStatus();
				if(status<0){
					System.err.println("I lost");
					break;
				}
				else if(status>0){
					System.err.println("I won");
					break;
				}
			}
			else{
				int res = p.getOpponentMove();
				if(res == -1)
					System.err.println("The server is messed up");
				else if (res == 1){
					System.err.println("The other player won.");
					break;
				}
				else 
					System.err.println("OK lemme think...");	
					
			}
            System.err.println(p.getBoard().toString('*'));
			turn = 3-turn;
		}	
	}
}
