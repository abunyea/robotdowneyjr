import java.util.HashSet;
import java.util.Scanner;

public class RandomAI extends Player {
	
	public RandomAI(Scanner scanner) {
		super(scanner);
	}

	@Override
	public String think() {
		int k=1;
		int retr1=-1,retr2=-1,retc1=-1,retc2=-1;
		Board board = getBoard();
		HashSet<Integer> hs = new HashSet<Integer>();
		// determine whether to add special marble or not. 
		int retr3 = -1;
		int retc3 = -1;
		if (board.canSetSpecialMarble(getMyturn())) {
			double num = Math.random();
			if (num < 0.4) {
				while (retr3 == -1 && retc3 == -1) {
					int i = (int) (Math.random() * 8 + 4);
					int j = (int) (Math.random() * 8 + 8);
					if (board.at(i,j) == 0) {
						retr3 = i;
						retc3 = j;
						board.setSpecialMarble(getMyturn(),retr3,retc3);
					}
				}
			}
		}

		//Find all my marbles
		for(int i=0; i<17; i++){
			for (int j = 0; j < 25; j++) {
				if(board.at(i, j) == getMyturn()){
					board.legalMoves(i,j,hs);
					for (int dest: hs){
						int r = dest/25;
						int c = dest%25;
						if (Board.isForward(i,j,r,c, getMyturn())){
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
		//perform the move before sending it
		Move m = new Move(0, 0, 0, retr1,retc1, retr2,retc2, retr3,retc3);
		board.move(m);
		return retr1+" "+retc1+" "+retr2+" "+retc2 + " "+ retr3+" "+ retc3;
	}

	public static void main(String args[]){
		RandomAI p = new RandomAI(new Scanner(System.in));
		int turn = p.getTurn();
		while (true)
		{
			System.err.println("turn = "+turn+"   myturn = "+p.getMyturn());	
			if (turn == p.getMyturn()){
				System.err.println("It is my turn and I am thinking");	
                String s=p.think();
				System.out.println(s);
				System.err.println("I sent: "+s);	
				int status = p.getStatus();
				System.err.println("I got: "+status);	
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
				//if (p.opponentWin){
				//	System.err.println("My opponent has won");
				//	break;
				//}
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
