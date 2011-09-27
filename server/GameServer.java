import java.io.PrintWriter;
import java.util.Calendar;
import java.io.FileNotFoundException;

public class GameServer {

	private int[] time = new int[2];
	private int[] specialsLeft = new int[2];
	private TimedReader[] selector = new TimedReader[2];
	private PrintWriter[] pw = new PrintWriter[2];
	private PrintWriter log;
    private int[] allotedTime = new int[2];
	int turn = 0;
	Board board;

	public GameServer(String logfile, String initialBoard, ConnectionThread player1Thread, ConnectionThread player2Thread) {
        board = new Board(initialBoard);
		selector[0] = player1Thread.getSelector();
		pw[0] = player1Thread.getPrintWriter();
		selector[1] = player2Thread.getSelector();
		pw[1] = player2Thread.getPrintWriter();
		time[0] = board.getTimeLimit(1);
		time[1] = board.getTimeLimit(2);
        allotedTime[0] = time[0];
        allotedTime[1] = time[1];
		specialsLeft[0] = board.getSpecials(1);
		specialsLeft[1] = board.getSpecials(2);
        turn = board.getTurn()-1;
		try {
			log = new PrintWriter(logfile);
            board.setLog(log); 
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		setupGame();
	}

	private void setupGame() {
	    String createdBoard = board.toString('0');
		for (int i = 0; i < 2; i++) {
			pw[i].write(createdBoard);
			//send each player their turn
			pw[i].write(""+(i+1) +"\n");
			pw[i].flush();
		}
		System.out.println(createdBoard);
	}

	private int play() {
		String[] s = new String[2];
		int stepSize = 200;
		StringBuilder sb = new StringBuilder();
		int winner = -1;
		int newstatus = -1;

		while (winner == -1) {
			sb.delete(0, sb.length());
			s[turn] = "";
			String currentS = "";
			while (time[turn] > 0 && (currentS.equals("") || (currentS.charAt(currentS.length() - 1) != '\n'))) {
				Calendar beforeRead = Calendar.getInstance();
				long beforeTime= beforeRead.getTimeInMillis();
				currentS = selector[turn].timeoutRead(stepSize);
				Calendar afterRead = Calendar.getInstance();
				long usedTime = afterRead.getTimeInMillis()-beforeTime;
				sb.append(currentS);
				time[turn] -= usedTime;
				if (currentS == null) {
					log.println("#player" + (turn + 1) + "'s connection is lost");
					winner = 2-turn;
					break;
				}
                log.flush();
			}

			if (currentS != null)  {
				s[turn] = sb.toString().trim();
				int status = 0;
				if (time[turn]<=0){
					status = -3;
					log.println("#Using greedy player");
					Move m = GreedyAI.greedy(board, turn+1);
					s[turn] = m.r1+" "+m.c1+" "+m.r2+" "+m.c2+" "+m.r3+" "+m.c3;
					board.move(m);					
					if(board.checkWin(turn+1)){
						newstatus = 1;
					}
				}
				else{
					Move m = validate(s[turn],turn);
					if(m!=null){
						board.move(m);
						if(board.checkWin(turn+1)){
							status = 1;
						}
					}
					else{
						status = -2;
					}
				}
				
				int otherstatus =0;
				switch(status){
				case 0:
					otherstatus = 0;
					break;
				case 1:
					otherstatus = -1;
					winner = turn+1;
					break;
				case -1:
				case -2:					
					otherstatus = 1;
                    winner = 2-turn;
					break;
				case -3:
					otherstatus = 0;
					if(newstatus == 1){
						otherstatus = -1;
						winner = turn+1;
						}
					break;
				default:
					log.println("# we got a weird status");
					break;

				}
				
				log.println("#player "+(turn+1)+ " sent " + s[turn]);
				printBoard();
				System.out.println(status+" "+time[0]+" "+time[1]+" "+s[turn]);

				if(pw[1-turn]!=null){
					pw[1 - turn].write(otherstatus+" "+time[0]+" "+time[1]+" "+s[turn]+"\n");
					pw[1 - turn].flush();
				}
				if(pw[turn]!=null){
					pw[turn].write(status+"\n");
					pw[turn].flush();
				}
				if(status < 0 && pw[turn]!=null){
					pw[turn].close();
					pw[turn]=null;
				}
			}
			turn = 1 - turn;
		}
		log.println("#winner="+winner);
		log.println("#Time left for player1 is " + time[0]+ " player 2 " + time[1]);
		log.close();
        return winner;
	}

	private void printBoard() {
		log.println(board.toString('*'));
		System.out.println(board.toString2('*'));
	}
	
	private Move validate(String s, int myturn){
		// validate moves player output value should be (r1, c1)and (r2, c2) (just send us four numbers without any punctuation.
		// r1,c1 indicates the location of the marble you want to move. 
		// r2,c2 indicates the location this marble moves to
		String[] coordinates = s.split(" ");
		if (coordinates.length != 6) {
			System.out.println("#player sent too many/too few numbers");
            log.println("#player sent too many/too few numbers");
			return  null;
		}
		try {
			int r1 = Integer.parseInt(coordinates[0]);
			int c1 = Integer.parseInt(coordinates[1]);
			int r2 = Integer.parseInt(coordinates[2]);
			int c2 = Integer.parseInt(coordinates[3]);
			int r3 = Integer.parseInt(coordinates[4]);
			int c3 = Integer.parseInt(coordinates[5]);
			if(!board.validateSimpleMove(r1,c1,r2,c2,r3,c3,myturn+1)){
				return null;
			}
			return new Move(0, 0, 0, r1, c1, r2, c2, r3, c3);
		} catch (NumberFormatException e) {
			System.out.println("#player sent a non-number");
            log.println("#player sent a non-number");
			return null;
		}
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
        GameServer gs; 
        Process p1;
        Process p2;
        String initialBoard;
        int exit1=-1;
        int exit2=-1;
        int winner=-1;
        int port=-1;
        int maxmem=-1;
        Runtime r=Runtime.getRuntime();

        if(args.length < 7 || args.length > 8){
            System.out.println("Usage: java GameServer Port MaxMemory Player1StdErrFile Player2StdErrFile Player1Cmd Player2Cmd ServerLog [InitialBoard]");
            return;
        }
        if (args.length == 8)
            initialBoard = args[7];
        else
            initialBoard = null;

        try{
            port=Integer.parseInt(args[0]);
            if(port<1024 || port>65535){
                System.out.println("Invalid port number "+port);
                return;
            }
        }catch(Exception e){
            e.printStackTrace();
            return;
        }

        try{
            maxmem=Integer.parseInt(args[1]);
            /* Since some JVMs will not run if the address space is small
             * we ignore this for now and allow 2 Gigabytes. Which is the 
             * absolute maximum we could ever allow.
             */
            maxmem=2*1024*1024*1024-1;
        }catch(Exception e){
            e.printStackTrace();
            return;
        }

		ConnectionThread player1Thread = new ConnectionThread(port);
		ConnectionThread player2Thread = new ConnectionThread(port+2);
		player1Thread.start();
		player2Thread.start();
        try{
            Thread.sleep(1000);
        }catch(Exception e){
            e.printStackTrace();
            System.exit(1);
        }

        try{
            p1=r.exec("./stdwrap "+maxmem+" "+port+" "+args[2]+" "+args[4]);
            p2=r.exec("./stdwrap "+maxmem+" "+(port+2)+" "+args[3]+" "+args[5]);
        }catch(Exception e){
            e.printStackTrace();            
            System.exit(1);
            return;
        }

		try {
			player1Thread.join();
			player2Thread.join();
		} catch (InterruptedException e) {
			e.printStackTrace();
			return;
		}
		
		// Connections are established for both players.
		gs = new GameServer(args[6],initialBoard,player1Thread,player2Thread);
		winner=gs.play();
        try{
            Thread.sleep(1000);
        }catch(Exception e){
            e.printStackTrace();
        }
        p1.destroy();
        p2.destroy();
        try{
            exit1=p1.waitFor();
            exit2=p2.waitFor();
        }
        catch(InterruptedException e){
            exit1=-878;
            exit2=-878;
        }
        System.err.println("Allotted Time - Player 1: "+gs.allotedTime[0]+" Player 2: "+gs.allotedTime[1]);
        System.err.println("Player 1");
        System.err.println(gs.time[0]+" "+42);
        System.err.println("Exit value = "+exit1);
        System.err.println("Player 2");
        System.err.println(gs.time[1]+" "+42);
        System.err.println("Exit value = "+exit2);
        System.err.println(winner);
	}
}
