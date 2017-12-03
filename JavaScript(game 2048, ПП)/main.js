var POINT = function()
{
    this.x = 0;
    this.y = 0;
};

var object = function()
{   
    this.name = "NONE";
    this.pos = new POINT();   
    this.size = new POINT();    
    this.color = "rgba(255,255,255,1)";  
};

var gameSpace = function()
{
    object.call(this);
    this.draw = function (ctx)
    {
        ctx.fillStyle = this.color;
        ctx.fillRect(this.pos.x, this.pos.y, this.size.x, this.size.y);  
    } 
}; 

var tile = function()
{
    object.call(this);

    this.id = new POINT();
    if( Math.random() * 100 > 90) this.count = 4;
    else this.count = 2;

    this.draw = function (ctx)
    {
        ctx.fillStyle = "rgba(255,255,255,0.1)";  
        ctx.fillRect(this.pos.x, this.pos.y, this.size.x, this.size.y);

        ctx.textAlign = "center";
        ctx.fillStyle = "rgba(50,50,50,0.5)";  
        ctx.font = "normal "+ (this.size.y / 1.5) +"pt Arial";
        ctx.fillText(this.count, this.pos.x + this.size.x / 2, this.pos.y + this.size.y / 2 + (this.size.y / 1.5) / 2, this.size.x);
    } 
}

var scene = function ()
{
    var dt = new Date();
    this.time = dt.getTime();
    this.bgColor = "rgba(200,200,200,1)";
    this.objects = new Array();
    this.moveSpeed = 50;
    this.add = false;
    this.listen = true;

    this.gameState = 0;

    this.draw = function (ctx)
    {
        ctx.canvas.width = window.innerWidth;
        ctx.canvas.height = window.innerHeight;
        this.moveSpeed = ctx.canvas.height / 10;

        ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
        ctx.fillStyle = this.bgColor;
        ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);

        var newTime = new Date();
        var dt = (newTime.getTime() - this.time) / 100;
        this.time = newTime.getTime();

        this.listen = true;
        
        ctx.textAlign = "left";
        ctx.fillStyle = "rgba(50,50,50,0.5)";  
        ctx.font = "normal "+ 20 +"pt Arial";
        ctx.fillText("R - to restart", 10, 40);
        ctx.fillText("Arrows - to move tiles", 10, 80);
        for(var i = 0; i < this.objects.length; i++)
        {
            this.objects[i].draw(ctx); 

            if(this.objects[i].name == "TILE")
            {
                if(this.objects[i].count > 2048)
                    this.gameState = 1;

                if(this.move(i, dt) == true) 
                    this.listen = false;
            }
        }   
        if(this.gameState == 1)
        {
            ctx.textAlign = "center";
            ctx.fillStyle = "rgba(50,50,50,0.5)";  
            ctx.font = "normal "+ 100 +"pt Arial";
            ctx.fillText("You won!", ctx.canvas.width/2, ctx.canvas.height/2);
        }
        if(this.listen && this.add)  {this.createRndTile(); this.add = false;}
    };
    this.logic = function (keyCode)
    {
        if(this.listen)
            switch (keyCode)
            {
                case 37: //left
                {
                    var shift = true;
                    var countLoop = 0;
                    while (shift)
                    {
                        countLoop++;
                        shift = false;
                        for(var i = 0; i < this.objects.length; i++)
                        {
                            if(this.objects[i].name == "TILE")
                            {
                                if(this.objects[i].id.x > 0)
                                {
                                    var id_obj = this.getObjectByPos(this.objects[i].id.x - 1, this.objects[i].id.y);
                                    if(id_obj == -1)
                                    {
                                        this.objects[i].id.x -= 1;
                                        shift = true;
                                    }
                                    else
                                    {
                                        if(this.objects[id_obj].count == this.objects[i].count)
                                        {
                                            this.objects[i].count *= 2;
                                            this.objects.splice(id_obj, 1);

                                            shift = true;
                                            i = -1;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if(countLoop > 1)
                        this.add = true; 
                    break;
                }
                case 38: //up
                {
                    var shift = true;
                    var countLoop = 0;
                    while (shift)
                    {
                        countLoop++;
                        shift = false;
                        for(var i = 0; i < this.objects.length; i++)
                        {
                            if(this.objects[i].name == "TILE")
                            {
                                if(this.objects[i].id.y > 0)
                                {
                                    var id_obj = this.getObjectByPos(this.objects[i].id.x, this.objects[i].id.y - 1);
                                    if(id_obj == -1)
                                    {
                                        this.objects[i].id.y -= 1;
                                        shift = true;
                                    }
                                    else
                                    {
                                        if(this.objects[id_obj].count == this.objects[i].count)
                                        {
                                            this.objects[i].count *= 2;
                                            this.objects.splice(id_obj, 1);

                                            shift = true;
                                            i = -1;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if(countLoop > 1)
                        this.add = true; 
                    break;
                }
                case 39: // right
                {
                    var shift = true;
                    var countLoop = 0;
                    while (shift)
                    {
                        countLoop++;
                        shift = false;
                        for(var i = 0; i < this.objects.length; i++)
                        {
                            if(this.objects[i].name == "TILE")
                            {
                                if(this.objects[i].id.x < 3)
                                {
                                    var id_obj = this.getObjectByPos(this.objects[i].id.x + 1, this.objects[i].id.y);
                                    if(id_obj == -1)
                                    {
                                        this.objects[i].id.x += 1;
                                        shift = true;
                                    }
                                    else
                                    {
                                        if(this.objects[id_obj].count == this.objects[i].count)
                                        {
                                            this.objects[i].count *= 2;
                                            this.objects.splice(id_obj, 1);

                                            shift = true;
                                            i = -1;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if(countLoop > 1)
                        this.add = true; 
                    break;
                }
                case 40: // down
                {
                    var shift = true;
                    var countLoop = 0;
                    while (shift)
                    {
                        countLoop++;
                        shift = false;
                        for(var i = 0; i < this.objects.length; i++)
                        {
                            if(this.objects[i].name == "TILE")
                            {
                                if(this.objects[i].id.y < 3)
                                {
                                    var id_obj = this.getObjectByPos(this.objects[i].id.x, this.objects[i].id.y + 1);
                                    if(id_obj == -1)
                                    {
                                        this.objects[i].id.y += 1;
                                        shift = true;
                                    }
                                    else
                                    {
                                        if(this.objects[id_obj].count == this.objects[i].count)
                                        {
                                            this.objects[i].count *= 2;
                                            this.objects.splice(id_obj, 1);

                                            shift = true;
                                            i = -1;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if(countLoop > 1)
                        this.add = true;
                    break;
                }
                case 82: // R
                {
                    var c = document.getElementById("main_canvas");
	                var ctx = c.getContext('2d');
                    this.startGame(ctx);
                    break;
                }
            }
    };
    this.move = function (id, dt)
    {
        var move_active = false;
        var field;
        for(var it = 0; it < this.objects.length; it++)
            if(this.objects[it].name == "FIELD") field = this.objects[it];
        if(!!!field) return;

        var point = new POINT();
        point.x = field.pos.x + ((this.objects[id].size.x + 10) * this.objects[id].id.x) + 10;
        point.y = field.pos.y + ((this.objects[id].size.y + 10) * this.objects[id].id.y) + 10;

        if(this.objects[id].pos.x != point.x)
        {
            move_active = true;
            if(point.x - this.objects[id].pos.x > 0)
            {
                this.objects[id].pos.x += this.moveSpeed * dt;
            }
            else
            {
                this.objects[id].pos.x -= this.moveSpeed * dt;
            }
        }
        if(this.objects[id].pos.y !=  point.y)
        {
            move_active = true;
            if(point.y - this.objects[id].pos.y > 0)
            {
                this.objects[id].pos.y += this.moveSpeed * dt;
            }
            else
            {
                this.objects[id].pos.y -= this.moveSpeed * dt;
            }
        }

        if(Math.abs(this.objects[id].pos.y - point.y) <= (this.moveSpeed * dt + 0.1))
        {
            this.objects[id].pos.y = point.y;
        }
        if(Math.abs(this.objects[id].pos.x - point.x) <= (this.moveSpeed  * dt + 0.1))
        {
            this.objects[id].pos.x = point.x;
        }
        return move_active;
    }
    this.startGame = function (ctx)
    {
        ctx.canvas.width = window.innerWidth;
        ctx.canvas.height = window.innerHeight;
        this.gameState = 0;

        this.objects = []; //Erase array

        var field = new gameSpace();   
        field.name = "FIELD";
        field.color = "rgba(100,100,100,0.5)";  
        field.size.x = ctx.canvas.height / 1.2;
        field.size.y = ctx.canvas.height / 1.2;
        field.pos.x = ctx.canvas.width / 2 - field.size.x / 2;
        field.pos.y = ctx.canvas.height / 2 - field.size.y / 2;
        this.objects[this.objects.length] = field;
    
        for(var i = 0; i < 2; i++)
        {             
            this.createRndTile();
        }
    }
    this.createTile = function(i, j)
    {
        var field;
        for(var it = 0; it < this.objects.length; it++)
            if(this.objects[it].name == "FIELD") field = this.objects[it];

        if(!!!field) return;

        var tl = new tile(); 
        tl.id.x = i; 
        tl.id.y = j; 
        tl.name = "TILE";
        tl.color = "rgba(255, 255, 255, 0.9)";  
        tl.size.x = field.size.x / 4 - 12.5;
        tl.size.y = field.size.x / 4 - 12.5;
        tl.pos.x = field.pos.x + ((tl.size.x + 10) * i) + 10;
        tl.pos.y = field.pos.y + ((tl.size.y + 10) * j) + 10;
        this.objects[this.objects.length] = tl;        
    }
    this.relocateTile = function(id, i, j)
    {
        var field;
        for(var it = 0; it < this.objects.length; it++)
            if(this.objects[it].name == "FIELD") field = this.objects[it];

        if(!!!field) return;

        var tl = this.objects[id];
        tl.id.x = i; 
        tl.id.y = j; 
    }
    this.getObjectByPos = function(i, j)
    {
        for(var it = 0; it < this.objects.length; it++)
        {
            if(this.objects[it].name == "TILE" && this.objects[it].id.x == i && this.objects[it].id.y == j)
                return it;
        }
        return -1;
    }
    this.createRndTile = function()
    {
        var countTiles = 16 - (this.objects.length - 1);
        if(countTiles == 0) return 0;
        var loc_pos = Math.floor((Math.random() * countTiles));    

        for(var j = 0; j < 4; j++)
            for(var k = 0; k < 4; k++)
            {
                if(this.getObjectByPos(j, k) == -1)
                { 
                    if(loc_pos == 0) { this.createTile( j, k ); return 0;}
                    loc_pos -= 1;
                }
            }
    }
};


window.onload = function()
{
    var btn_click = true;
    var c = document.getElementById("main_canvas");
	var ctx = c.getContext('2d');

    window.addEventListener("keydown", handlekeydown, true);
    window.addEventListener("keyup", handlekeyup, true);

    setInterval(loop, 1000/70);

    var sc = new scene();
    sc.startGame(ctx);

    function loop()
    {
        sc.draw(ctx);
    }
    function handlekeydown(input)
    {
        if(btn_click)
            sc.logic(input.keyCode);
        console.log(input.keyCode);
        btn_click = false;       
    }
    function handlekeyup(input)
    {
        btn_click = true;       
    }
}
