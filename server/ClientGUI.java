import java.awt.EventQueue;

import javax.imageio.ImageIO;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.JLabel;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Image;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.HashSet;
import java.util.Scanner;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.ButtonGroup;
import javax.swing.Timer;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JMenuItem;
import javax.swing.JButton;

@SuppressWarnings("serial")
class GUIBoard extends JPanel{
	
	/* Game logic */
	Board board;
	private int selecti;
	private int selectj;
	private int speciali;
	private int specialj;
	HashSet<Integer> validMoves;
	int turn;
	int[] time;
	boolean gameover;
	GameTranscript transcript;
	/* GUI stuff */
	private static Image boardimg;
	private static Image[] marble;
	private static Image selectimg;
	private static Image moveimg;
    private static final int xOff = 16, yOff = 26;
    private static final int xScale = 16, yScale = 24;
    
	private void drawMarbles(Graphics g){
		for(int i=0; i<17; i++){
			for(int j=0; j<25; j++){
				if(board.at(i,j)>0)
					g.drawImage(marble[board.at(i,j)-1],xOff+j*xScale,yOff+i*yScale,this);
			}
		}
	}
	
	private void drawTurn(Graphics g){
		if (turn==1){
			g.drawImage(marble[turn-1],xOff+0*xScale,yOff+0*yScale,this);
		}
		else {
			g.drawImage(marble[turn-1],xOff+0*xScale,yOff+1*yScale,this);
		}
		for(int yy=0; yy<2; yy++){
			for(int xx=1; xx<=board.specialsLeft[yy]; xx++){
				g.drawImage(marble[2],xOff+xx*xScale,yOff+yy*yScale,this);
			}
		}
	}
	
	private void drawSelectionAndMoves(Graphics g){
		if (selecti>0 || selectj>0){
			g.drawImage(selectimg,xOff+selectj*xScale,yOff+selecti*yScale,this);
			for (int m: validMoves){
				int i=m/25;
				int j=m%25;
				g.drawImage(moveimg,xOff+j*xScale,yOff+i*yScale,this);
			}
		}		
	}
	
	public void setSelection(int i, int j){
		if(board.at(i, j)<=0 || board.at(i, j)!=turn || gameover){
			selecti=0;
			selectj=0;
			validMoves.clear();
		}
		else if (selecti!=i || selectj!=j){
			selecti=i;
			selectj=j;
			board.legalMoves(selecti, selectj, validMoves);
		}
	}
	
	public boolean setSpecial(int i, int j) {
		if(i<4 || i>12)
			return false;
		if(!board.canSetSpecialMarble(turn))
			return false;
		if(board.at(i, j)!=0 || gameover){
			speciali=-1;
			specialj=-1;
			return false;
		}
		else {
			speciali=i;
			specialj=j;
			board.setSpecialMarble(turn, speciali, specialj);
			return true;
		}
	}

	public static void mouseToIdx(int x, int y, int[] idx) {
        idx[0] = (y-yOff)/yScale;
        idx[1] = (x-xOff)/xScale;
	}
	
	public GUIBoard() {
		super();
		marble = new Image[3];
		board = new Board(null);
		time = new int[] {6000, 6000};
		turn = 1;
		gameover = false;
		validMoves = new HashSet<Integer>();

		if (boardimg==null){
			try {
				boardimg = ImageIO.read(new File("board.gif"));
				marble[0] = ImageIO.read(new File("red.gif"));
				marble[1] = ImageIO.read(new File("blue.gif"));
				marble[2] = ImageIO.read(new File("gray.gif"));
				selectimg = ImageIO.read(new File("highlight.gif"));
				moveimg = ImageIO.read(new File("moves.gif"));
				setPreferredSize(new Dimension(boardimg.getWidth(null),boardimg.getHeight(null)));
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		g.drawImage(boardimg, 0, 0, this);
		drawMarbles(g);
		if(!gameover){
			drawTurn(g);
			drawSelectionAndMoves(g);
		}
	}

	/* Check if the player whose turn just passed make a move that wins the game.
	 * To do this check the home positions of the other player (the one whose turn is)  
	 */
	public void checkWin() {
		board.checkWin(turn);
	}
	
	public boolean isGameover() {
		return gameover;
	}

	public int getTurn() {
		return turn;
	}

    public void loadGame(File f) {
        GameTranscript g = new GameTranscript(); 
        Scanner sc;
        String line;
        boolean corrupt = false;
        try {
            sc = new Scanner(f);
            if(!board.readConfig(sc)){
                corrupt = true;
            }
            if(!sc.hasNextLine())
                corrupt = true;
            while(!corrupt && sc.hasNextLine()){
                line = sc.nextLine();
                if (line.length()==0 || line.charAt(0)=='#')
                    continue;
                g.add(line);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            corrupt = true;
        }
        if(!corrupt){
            transcript = g;
        }
    }

    public Move advance() {
        if (transcript == null) return null;
        return drawMove(transcript.next());
    }

    public Move retract() {
        if (transcript == null) return null;
        return drawBackwardMove(transcript.previous());
    }

    private Move drawMove(String s) {
        if(s==null) return null;
        Move m = board.validateMove(s, turn);
        if (m==null) return null;
        selecti = m.r1;
        selectj = m.c1;
        speciali = m.r3;
        specialj = m.c3;
        validMoves.clear();
        validMoves.add(25*m.r2+m.c2);
        board.move(m);
        turn=3-turn;
        return m;
    }

    private Move drawBackwardMove(String s) {
        if(s==null) return null;
        Move m = board.validateBackwardMove(s, turn);
        if (m==null) return null;
        selecti = m.r2;
        selectj = m.c2;
        speciali = m.r3;
        specialj = m.c3;
        validMoves.clear();
        validMoves.add(25*m.r1+m.c1);
        board.backwardMove(m);
        turn=3-turn;
        return m;
    }

    public void clearSpecial() {
        speciali=-1;
        specialj=-1;
    }

    public boolean attemptMove(int i, int j) {
        if(board.validateSimpleMove(selecti, selectj, i, j, speciali, specialj, turn)){
            board.move(new Move(0,0,0, selecti, selectj, i, j, speciali, specialj));
            clearSpecial();
            return true;
        }
        return false;
    }
}

public class ClientGUI {

    private JFrame frame;
    private JFileChooser fc;

    /**
     * Launch the application.
     */
    public static void main(String[] args) {
        EventQueue.invokeLater(new Runnable() {
            public void run() {
                try {
                    ClientGUI window = new ClientGUI();
                    window.frame.setVisible(true);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
    }

    /**
     * Create the application.
     */
    public ClientGUI() {
        initialize();
    }

    /**
     * Initialize the contents of the frame.
     */
    private void initialize() {
        fc = new JFileChooser();
        fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

        frame = new JFrame();
        frame.setResizable(false);
        frame.setBounds(100, 100, 317, 277);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        JMenuBar menuBar = new JMenuBar();
        frame.setJMenuBar(menuBar);

        final JMenu mnFile = new JMenu("File");
        menuBar.add(mnFile);

        final JMenuItem mntmLoadGame = new JMenuItem("Load Game");

        mnFile.add(mntmLoadGame);

        JPanel playPanel = new JPanel();
        frame.getContentPane().add(playPanel, BorderLayout.SOUTH);

        JButton btnPrevious = new JButton("<");
        playPanel.add(btnPrevious);

        JButton btnNext = new JButton(">");
        playPanel.add(btnNext);

        JPanel timepanel = new JPanel();
        frame.getContentPane().add(timepanel, BorderLayout.NORTH);
        timepanel.setLayout(new GridLayout(0, 4, 0, 0));

        JLabel lblRed = new JLabel("Red:");
        timepanel.add(lblRed);

        final JLabel lblRedtimeleft = new JLabel("10:00");
        timepanel.add(lblRedtimeleft);

        JLabel lblBlue = new JLabel("Blue:");
        timepanel.add(lblBlue);

        final JLabel lblBluetimeleft = new JLabel("10:00");
        timepanel.add(lblBluetimeleft);		

        final GUIBoard boardpanel = new GUIBoard();

        mntmLoadGame.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                if(fc.showOpenDialog(mntmLoadGame)==JFileChooser.APPROVE_OPTION){
                    File f=fc.getSelectedFile();
                    boardpanel.loadGame(f);
                    mnFile.setEnabled(false);
                    boardpanel.repaint();
                }
            }
        });

        boardpanel.addMouseListener(new MouseAdapter() {
            private boolean specialUsed;

            @Override
            public void mouseClicked(MouseEvent e) {
                int x= e.getX();
                int y= e.getY();
                int idx[] = new int[2];
                GUIBoard.mouseToIdx(x,y,idx);
                //System.out.println("Click! at "+x+" "+y+" ["+idx[0]+","+idx[1]+"] "+e.getButton());
                if(idx[0]<0 || idx[0]>=17 || idx[1]<0 || idx[1]>=25)
            return;
        switch(e.getButton()){
            case MouseEvent.BUTTON1:
                boardpanel.setSelection(idx[0], idx[1]);
                break;
                /*
                   case MouseEvent.BUTTON2:
                   if(!specialUsed && boardpanel.setSpecial(idx[0], idx[1])){
                   specialUsed=true;
                   }
                   break;
                   case MouseEvent.BUTTON3:
                   if (boardpanel.attemptMove(idx[0], idx[1])){
                   boardpanel.checkWin();
                   specialUsed=false;
                   }
                   break;
                   */
        }
        boardpanel.repaint();
            }
        });

        btnNext.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                Move m=boardpanel.advance();
                if(m==null) return;
                if(m.status==1){
                    if(boardpanel.getTurn()==1){
                        lblRedtimeleft.setText("Lose!");
                        lblBluetimeleft.setText("Win!");
                    }
                    else{
                        lblRedtimeleft.setText("Win!");
                        lblBluetimeleft.setText("Lose!");
                    }
                }
                else{
                    String rs = String.format("%02d", (m.t1/1000)%60);
                    String bs = String.format("%02d", (m.t2/1000)%60);
                    lblRedtimeleft.setText (""+(m.t1/60000)+":"+rs+"."+(m.t1%1000));
                    lblBluetimeleft.setText(""+(m.t2/60000)+":"+bs+"."+(m.t2%1000));
                }
                boardpanel.repaint();
            }
        });

        btnPrevious.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                Move m=boardpanel.retract();
                if(m==null) return;
                String rs = String.format("%02d", (m.t1/1000)%60);
                String bs = String.format("%02d", (m.t2/1000)%60);
                lblRedtimeleft.setText (""+(m.t1/60000)+":"+rs+"."+(m.t1%1000));
                lblBluetimeleft.setText(""+(m.t2/60000)+":"+bs+"."+(m.t2%1000));
                boardpanel.repaint();
            }

        });

        frame.getContentPane().add(boardpanel, BorderLayout.CENTER);
        frame.pack();
    }
}
