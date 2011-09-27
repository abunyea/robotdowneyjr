import java.util.Scanner;

public abstract class Player {
	
	Board board;
	
	private int myturn;
	//public boolean opponentWin = false;
	private Scanner sc;
	
	public Player(Scanner sc){
        board = new Board(null);
		this.sc = sc;
		readConfig();
	}
	
	public void readConfig(){
		if(!board.readConfig(sc)){
			System.err.println("Did not receive initial board. Exiting...");
			System.exit(1);
		}
		if(sc.hasNextLine()){
			try{
				myturn = Integer.parseInt(sc.nextLine());
			}catch(NumberFormatException e){
				System.err.println("Could not parse turn. Exiting...");
				System.exit(1);
			}
		}
		else{
			System.err.println("Did not receive turn. Exiting...");
			System.exit(1);
		}
	}
	
	public int getOpponentMove(){
		if(!sc.hasNextLine()){
			System.err.println("Input was closed unexpectedly. Exiting...");
			System.exit(1);
		}
		String oppMove = sc.nextLine();
		Move m = board.validateMove(oppMove,3-myturn);
		if (m!=null){
			board.move(m);
			boolean winner = board.checkWin(3-myturn);
			if (winner)
				return 1;
			else
				return 0;
		}
		else{
			return -1;
		}
	}
	
	public int getStatus() {
		if(sc.hasNextLine()){
			String status = sc.nextLine();
			int s = Integer.parseInt(status);
			return s;
		}
		return 0;
	}
	
	public abstract String think() ;
	
	public Board getBoard() {
		return board;
	}

	public int getMyturn() {
		return myturn;
	}

	public int getTurn() {
		return board.getTurn();
	}

}
