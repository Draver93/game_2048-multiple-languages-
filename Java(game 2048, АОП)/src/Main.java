import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.ActionEvent;

import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;

import javax.swing.*;
import java.util.*;
import java.lang.String;
import java.util.ArrayList;
import java.util.Random;

import javax.swing.Timer;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.reflect.MethodSignature;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;

class POINT 
{
	public int x, y;
	POINT()
	{
		x = 0;
		y = 0;
	}
};

class object
{
	public String name;
	public POINT pos;
	public POINT size;
	public int color = 0xffffffff;
	
    object()
    {
    	name = "NONE";
    	pos = new POINT();
    	size = new POINT(); 	
    }
};
class gameSpace extends object
{
	void draw(Graphics g)
	{
		Graphics2D g2d = (Graphics2D)g;
		
		g2d.setColor(new Color(255,255,255));
    	g2d.fillRect(pos.x, pos.y, size.x, size.y);
	}
}; 

class tile extends object
{
	public POINT id;
    public int count;

    tile()
    {
    	id = new POINT();
    	final Random random = new Random();
    	if( random.nextInt(100) > 90) count = 4;
        else count = 2;
    }
    void draw(Graphics g)
	{
    	Graphics2D g2d = (Graphics2D)g;
    	
    	g2d.setColor(new Color(255,100,100));
    	g2d.fillRect(pos.x, pos.y, size.x, size.y);
    	
    	g2d.setColor(new Color(50,50,50));
        Font ft = new Font("TimesRoman", Font.PLAIN, (int) (size.y / 1.5));      
        g2d.setFont(ft);
        Rectangle rc = new Rectangle(pos.x*2,pos.y*2,(pos.x + size.x/2)*2,( pos.y + size.y/ 2) * 2);
        
        scene.drawCenteredString(g2d, String.valueOf(count), rc, ft);
	}
}
class scene 
{
	public long time;
	public int []bgColor = {255,255,255,1};
	public int moveSpeed;
	public int gameState;
	public boolean add;
	public boolean listen;
	public String str;
	ArrayList<tile> tiles;
	gameSpace gs;
	
    scene()
    {
    	time = System.currentTimeMillis();
    	moveSpeed = 50;
    	gameState = 0;
    	add = false;
    	listen = true;
    	tiles = new ArrayList<tile>();
    	gs = new gameSpace();
    	str = "2222";
    
    }
    public void draw(Graphics g)
	{
    	Graphics2D g2d = (Graphics2D)g;
    	
        Color cl = new Color(255, 0, 0);
        cl = new Color(100,100,100);
                
       	float dt = (System.currentTimeMillis() - time) / 15;
        time = System.currentTimeMillis();
     
        gs.draw(g2d);
        listen = true;
        for(int i = 0; i < tiles.size(); i++)
        {
        	tile tmpTl = tiles.get(i); 
        	tmpTl.draw(g2d);
            if(tmpTl.count > 2048)
                gameState = 1;

            if(move(i, dt) == true) 
                listen = false;
        }   
        Font ft = new Font("TimesRoman", Font.PLAIN, 20);      
        g2d.setFont(ft);
        g2d.setColor(new Color(50,50,50));        
        g2d.drawString("R - to restart", 10, 20);
        g2d.drawString("Arrows - to move tiles", 10, 40);
    	ft = new Font("TimesRoman", Font.PLAIN, 10);    
        g2d.setFont(ft);
        g2d.drawString("Если игра не реагирует сделайте окно активным", 500, 580);
        if(this.gameState == 1)
        {
        	ft = new Font("TimesRoman", Font.PLAIN, 120);    
        	g2d.setFont(ft);     
            g2d.setColor(new Color(250,150,50)); 
            Rectangle rc = new Rectangle(0,0,Display.content.getWidth(),Display.content.getHeight());          
            scene.drawCenteredString(g2d,"You won!", rc, ft);
        }
        if(listen && add)  
        {
        	createRndTile(); 
        	add = false;
        }
	}
    public void logic(KeyEvent e)
    {
    	if(listen)
            switch (e.getKeyCode())
            {
                case 37: //left
                {
                    boolean shift = true;
                    int countLoop = 0;
                    while (shift)
                    {
                        countLoop++;
                        shift = false;
                        for(int i = 0; i < tiles.size(); i++)
                        {
                        	tile tl = tiles.get(i);
                            if(tl.id.x > 0)
                            {
                                int id_obj = getObjectByPos(tl.id.x - 1, tl.id.y);
                                if(id_obj == -1)
                                {
                                	tl.id.x -= 1;
                                    shift = true;
                                }
                                else
                                {
                                	tile tl_sec = tiles.get(id_obj);
                                    if(tl_sec.count == tl.count)
                                    {
                                    	tl.count *= 2;
                                    	
                                    	tiles.remove(id_obj);

                                        shift = true;
                                        i = -1;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    if(countLoop > 1) add = true; 
                    break;
                }
                case 38: //up
                {
                	 boolean shift = true;
                     int countLoop = 0;
                     while (shift)
                     {
                         countLoop++;
                         shift = false;
                         for(int i = 0; i < tiles.size(); i++)
                         {
                         	tile tl = tiles.get(i);
                             if(tl.id.y > 0)
                             {
                                 int id_obj = getObjectByPos(tl.id.x, tl.id.y - 1);
                                 if(id_obj == -1)
                                 {
                                 	tl.id.y -= 1;
                                     shift = true;
                                 }
                                 else
                                 {
                                 	tile tl_sec = tiles.get(id_obj);
                                     if(tl_sec.count == tl.count)
                                     {
                                     	tl.count *= 2;
                                     	
                                     	tiles.remove(id_obj);

                                         shift = true;
                                         i = -1;
                                         break;
                                     }
                                 }
                             }
                         }
                     }
                     if(countLoop > 1) add = true; 
                     break;
                }
                case 39: // right
                {
                	 boolean shift = true;
                     int countLoop = 0;
                     while (shift)
                     {
                         countLoop++;
                         shift = false;
                         for(int i = 0; i < tiles.size(); i++)
                         {
                         	tile tl = tiles.get(i);
                             if(tl.id.x < 3)
                             {
                                 int id_obj = getObjectByPos(tl.id.x + 1, tl.id.y);
                                 if(id_obj == -1)
                                 {
                                 	tl.id.x += 1;
                                     shift = true;
                                 }
                                 else
                                 {
                                 	tile tl_sec = tiles.get(id_obj);
                                     if(tl_sec.count == tl.count)
                                     {
                                     	tl.count *= 2;
                                     	
                                     	tiles.remove(id_obj);

                                         shift = true;
                                         i = -1;
                                         break;
                                     }
                                 }
                             }
                         }
                     }
                     if(countLoop > 1) add = true; 
                     break;
                }
                case 40: // down
                {
                	 boolean shift = true;
                     int countLoop = 0;
                     while (shift)
                     {
                         countLoop++;
                         shift = false;
                         for(int i = 0; i < tiles.size(); i++)
                         {
                         	tile tl = tiles.get(i);
                             if(tl.id.y < 3)
                             {
                                 int id_obj = getObjectByPos(tl.id.x, tl.id.y + 1);
                                 if(id_obj == -1)
                                 {
                                 	tl.id.y += 1;
                                     shift = true;
                                 }
                                 else
                                 {
                                 	tile tl_sec = tiles.get(id_obj);
                                     if(tl_sec.count == tl.count)
                                     {
                                     	tl.count *= 2;
                                     	
                                     	tiles.remove(id_obj);

                                         shift = true;
                                         i = -1;
                                         break;
                                     }
                                 }
                             }
                         }
                     }
                     if(countLoop > 1) add = true; 
                     break;
                }
                case 82: // R
                {
                    this.startGame();
                    break;
                }
            }
    }
    public boolean move(int id, float dt)
    {
        boolean move_active = false;
        tile tmpTL = tiles.get(id);

        POINT point = new POINT();
        point.x = gs.pos.x + ((tmpTL.size.x + 10) * tmpTL.id.x) + 10;
        point.y = gs.pos.y + ((tmpTL.size.y + 10) * tmpTL.id.y) + 10;

        if(tmpTL.pos.x != point.x)
        {
            move_active = true;
            if(point.x - tmpTL.pos.x > 0)
            {
            	tmpTL.pos.x += moveSpeed * dt;
            }
            else
            {
            	tmpTL.pos.x -= moveSpeed * dt;
            }
        }
        if(tmpTL.pos.y !=  point.y)
        {
            move_active = true;
            if(point.y - tmpTL.pos.y > 0)
            {
            	tmpTL.pos.y += moveSpeed * dt;
            }
            else
            {
            	tmpTL.pos.y -= moveSpeed * dt;
            }
        }

        if(Math.abs(tmpTL.pos.y - point.y) <= (moveSpeed * dt + 0.1))
        {
        	tmpTL.pos.y = point.y;
        }
        if(Math.abs(tmpTL.pos.x - point.x) <= (moveSpeed  * dt + 0.1))
        {
        	tmpTL.pos.x = point.x;
        }
        return move_active;
    }
    public void createRndTile()
    {
    	int countTiles = 16 - tiles.size();
        if(countTiles == 0) return;
        final Random random = new Random();
        int loc_pos = random.nextInt(countTiles);    

        for(int j = 0; j < 4; j++)
            for(int k = 0; k < 4; k++)
            {
                if(this.getObjectByPos(j, k) == -1)
                { 
                    if(loc_pos == 0) { createTile( j, k ); return;}
                    loc_pos -= 1;
                }
            }
    }
    public void createTile(int i,int j)
    {	
		tile tl = new tile(); 
		tl.id.x = i; 
		tl.id.y = j; 
		tl.name = "TILE";
		tl.color = 0xffffffff;  
		tl.size.x = (int)(gs.size.x / 4 - 12.5);
		tl.size.y = (int)(gs.size.x / 4 - 12.5);
		tl.pos.x = gs.pos.x + ((tl.size.x + 10) * i) + 10;
		tl.pos.y = gs.pos.y + ((tl.size.y + 10) * j) + 10;
		tiles.add(tl);
    }
    public int getObjectByPos(int i, int j)
    {
        for(int it = 0; it < tiles.size(); it++)
        {
        	tile tmpTL = tiles.get(it);
            if(tmpTL.id.x == i && tmpTL.id.y == j)
                return it;
        }        
    	return -1;
    }
    public void startGame()
    {
        this.gameState = 0;
        tiles.clear();
        int height = Display.content.getHeight();
        int width = Display.content.getWidth();

        gs = new gameSpace();   
        gs.name = "FIELD";
        gs.color = 0xf0aaaaaa;  
        gs.size.x = (int)(height / 1.2);
        gs.size.y = (int)(height / 1.2);
        gs.pos.x = width / 2 - gs.size.x / 2;
        gs.pos.y = height / 2 - gs.size.y / 2;
    
        for(int i = 0; i < 2; i++)
        {             
            this.createRndTile();
        }
    }
    public void relocateTile(int id,int i,int j)

	{
	    tile tl = tiles.get(id);
	    tl.id.x = i; 
	    tl.id.y = j; 
	}

    public static void drawCenteredString(Graphics g, String text, Rectangle rect, Font font) 
    {
        // Get the FontMetrics
        FontMetrics metrics = g.getFontMetrics(font);
        // Determine the X coordinate for the text
        int x = (rect.width - metrics.stringWidth(text)) / 2;
        // Determine the Y coordinate for the text (note we add the ascent, as in java 2d 0 is top of the screen)
        int y = ((rect.height - metrics.getHeight()) / 2) + metrics.getAscent();
        // Set the font
        g.setFont(font);
        // Draw the String
        g.drawString(text, x, y);
        // Dispose the Graphics
    }
}


public class Main 
{
	public static void main(String[] args) 
	{       
        Display.create(800, 600, "2048", 0xdddddddd);
        
        Timer t = new Timer(1000 / 60, new AbstractAction() 
        {
        	public void actionPerformed(ActionEvent e)
        	{
        		Display.clear();
        		Display.render();
        		Display.swapBuffers();
        	}
        });
        t.setRepeats(true);
        t.start();
	}

}


class Display
{
	private static JFrame window;
	public static Canvas content;
	
	public static scene sc = new scene();
	
	private static BufferedImage buffer;
	private static int[] bufferData;
	private static Graphics bufferGraphics;
	private static int clearColor;
	
	private static boolean btn_click = true;
	
	public static void create(int width, int height, String title, int _clearColor)
	{
		window = new JFrame(title);
		window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		content = new Canvas();
		
		Dimension size =  new Dimension(width, height);
		content.setPreferredSize(size);
		
		KeyListener listener = new KeyListener() 
        {
        	public void keyPressed(KeyEvent e) 
        	{
        		if(btn_click)
                    sc.logic(e);
                btn_click = false;       
        	}
            public void keyReleased(KeyEvent e) 
            {
                btn_click = true;       
            }
            public void keyTyped(KeyEvent e) 
            {
            }
        };
        content.addKeyListener(listener);
        
		window.setResizable(false);
		window.getContentPane().add(content);
		window.pack();
		window.setLocationRelativeTo(null);
		window.setVisible(true);
		
		buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
		bufferData = ((DataBufferInt) buffer.getRaster().getDataBuffer()).getData();
		bufferGraphics = buffer.getGraphics();
		clearColor = _clearColor;
		sc.startGame();
	}
	public static void clear()
	{
		Arrays.fill(bufferData, clearColor);
	}
	public static void render()
	{
		sc.draw(bufferGraphics);
	}
	public static void swapBuffers()
	{
		Graphics g = content.getGraphics();
		g.drawImage(buffer, 0, 0, null);
	}
}


@Aspect
class logger {
	@Around("execution(* *(..))")
  	public Object around(ProceedingJoinPoint point) throws Throwable 
  	{
	    Object result = point.proceed();
	    String args = "";
	    Object[] signatureArgs = point.getArgs();
		for (Object signatureArg: signatureArgs) 
		{
		   args += signatureArg + " ";
		}
		if (args.length() == 0)
			args = "null ";
		String res = "Метод: " + MethodSignature.class.cast(point.getSignature()).getMethod().getName() + ". Аргументы: " + args + ". Результат: " + result;
	    System.out.println(res);
	    return result;
	  }
  
  	@AfterThrowing(pointcut = "execution(* *(..))", throwing = "ex")
	public void afterThrowingAdvice(Exception ex)  throws Exception 
	{
		  System.out.println(ex.getLocalizedMessage());
	}
  
}