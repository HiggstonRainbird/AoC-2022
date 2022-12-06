# Mathematica Advent of Code
# 2022

This repository stores my [Advent Of Code](http://adventofcode.com/2022/) solutions for 2022, written in Mathematica.  Some of these solutions are simple one-liners, and others are procedural and not fundamentally different from most other common programming languages, and so don't have any READMEs associated with them.  Some (marked in **bold**) are more involved and have (or need) explanations.  I'm using .wls instead of .m, and trying to make the code somewhat readable as plaintext, but casual Mathematica in general is notebook-heavy, and isn't great to view on GitHub.

There's also a utilities file, Ulitities.nb, with some functions that proved useful in previous years.  This is not a true utils file, in that it isn't a paclet and that I don't import the function definitions directly, but it's mostly there as boilerplate code I can copy in when need be.

## Days Completed

* Day 1: Counting Calories
* Day 2: Rock Paper Scissors
* Day 3: Rucksack Reorganization
* Day 4: Camp Cleanup
* Day 5: Supply Stacks

## Poems

### [Day 1](https://www.adventofcode.com/2022/day/1): Where's The Wild Things' Star?

We're now setting up camp  
In the dew and the damp,  
On the hunt for the star-spangled berry,  

And the jungle is thick,  
But we have every trick  
That will make the dark forest less scary,  

So we'll head to the grove  
With its heavenly trove  
(It's a Dijkstra-type maze, so be wary)  

Then the stars will appear,  
And when fifty are here,  
We will eat and we'll drink and be merry.  

### [Day 2](https://www.adventofcode.com/2022/day/2): To Be A Rock And Not To Roll

The match is set.  
The games decide  
Who's near the snacks,  
Who's near the tide.

The Rock is hard,  
But Paper's wide,  
And Scissor's good  
At Paper-cide.  

He gave a sheet  
(Which you have eyed)  
With every throw -  
You'll have to hide.  

You didn't know,  
But had your pride,  
And so you guessed  
The second side.  

When he returned,  
(His name is Clyde)  
He set you right  
(If he's allied).  

It's X to lose  
(Unless he lied),    
And Z to win,  
Or else, you've tied.  

You know the score.  
You have the guide.  
You'll win or lose  
(Unless you've Y'd).  

### [Day 3](https://www.adventofcode.com/2022/day/3): John '5space' Henry

When ol' John Henry was a babe,  
A-sitting on his papa's knee,  
He hammer-swung a tiny fist  
And hammer-hit a keyboard's key.  

They say that mighty Hercules,  
He wrestled pythons in his crib.  
John Henry, too, a Python slew;  
That's what they say, and they don't fib.  

When ol' John Henry was a boy,  
No more than eight (or maybe nine),  
His papa sat him down to code,  
And little John, he learned it fine.  

He typed so fast the frames would fail  
As terminal commands flew by.  
He typed so quick the keys would stick.  
That's what they say, and they don't lie.  

When ol' John Henry was a teen,  
His papa said the day'd arrived.  
"They call me `4space`", said his pop.  
"From here on out, they'll call you `5`."  

But smoke was rising from the hills,  
And filled with gray what once was blue:   
The code machine, it changed the scene.  
That's what they say, and they say true.  

When ol' John Henry was a man,  
And AoC was his domain,  
The code machine rolled up to fight  
And challenged him to end his reign.  

Part 1 it grabbed, ten seconds flat,  
Before John Henry knew what hit him,  
But on Part 2, it ground a gear,  
And Henry moved like crocs had bit him,  
'Cuz he was good and riled now,  
And wouldn't let some AI git him,  
With seven seconds left to go,  
He grabbed the star he knew would fit him.  

But Henry's heart had beat its last  
When he had beat his metal foe.  
They buried John, he's dead and gone.  
That's what they say, and they would know.  

### [Day 4](https://www.adventofcode.com/2022/day/4): A Series of Challenges  

*I had writer's block that morning, so I asked [ChatGPT](https://chat.openai.com/chat) to fill in for me and write that day's poem.*

Solving an Advent of Code problem  
Is like a treasure hunt or a quest,  
A series of challenges to overcome  
And a chance to put my skills to the test.  

I start by reading the problem statement  
And breaking it down into bits,  
Thinking about how to approach it  
And coming up with a plan that fits.  

Then I write some code to solve it  
Testing and debugging as I go,  
Making sure it's efficient and correct  
And ready for the next challenge, oh no.  

In the end, I submit my solution  
Feeling proud and accomplished too,  
Another Advent of Code problem solved  
And one step closer to being through.  

*I'm a bit impressed that it can write poetry at all...but I thought there was still some room for improvement.*  

As my character hunts through the jungle  
On a quest for a grove with a star,  
I myself, though I err and I bungle,  
Hunt for challenge, and that's what these are.  

For each problem, I start by not reading:  
Take a glance, make a guess at the rest.  
There's no time to slow down while I'm speeding  
And attempting to code with the best.  

Then I write down some code and submit it,  
Inefficient?  Ha!  *Sluggish*, I'll say.  
And the untested code, I admit it    
Sometimes works the first try, like today.  

In the end, I've had fun and competed,  
Having raced through the thing like a master,  
And with two shiny stars now completed,  
I can't help but think I could be faster.

### [Day 5](https://www.adventofcode.com/2022/day/5): Safety First (In Last Out)

A couple years ago, when it was winter,  
And Cratemover Two Thousand was the latest,  
When somebody was [busy with the printer](https://adventofcode.com/2017/day/1)  
And someone else took [*Breakout!*](https://adventofcode.com/2019/day/13) for a playtest,  
The wooden crates we had (and feared would splinter),  
Held presents, and the top crate was the greatest.  
Inside - what child wouldn't love to smack it? -  
Was bubble wrap with bubble wrap to pack it.  

If salt goes bad, with what can it be seasoned?  
If bug reports don't work, how would you know it?  
The wrap was safe on top (or so we reasoned),  
And surely safer than the stuff below it.  
I said "We wouldn't want the crate to squeeze" and  
"It's earned the place of honor" said the poet,  
And so our finest gift, our precious jewel  
Sat high atop the stack, as if to rule.  

The tallest stack, of course, sticks out the tallest,  
And that became a problem when a blizzard -  
The worst we had in years, in fact, the squall-est -  
Blew forth as if cast at us by a wizard.  
(The wizard's just one guess, though it's a small list;  
"Don't irk him, just in case" still sometimes is heard).  
We thought our stack of crates was doing well,  
Until at last, on Christmas Eve, it fell.  

Alas!  The bubble wrap had all been frozen  
Before it fell from such a risky height.  
The wind on top was strong; if we had chosen  
Some slightly smaller stacks, it'd be all right.  
It burst and shattered like it had a hose in;  
No children got to play with it that night.  
We're careful, now, when getting cargo loaded.  
We still remember what the ice and snow did.  

### [Day 6](https://www.adventofcode.com/2022/day/6): A Letter Back Home

    stftmtvvtvqqczqqnjnwwlqqdzdnnsvnsswb
    bwsstvvssfjsjbjfjmjpjzpplpppjzjqqdzz
    hqqqqtcccbzzzwzrrrdqdldpdsppmqmmnwwd
    dkddTHEmNOISEmrzahfpsczcyxbkpomcuuwj
    bmfhixwGETSlLOUDEReygvjqykvppgbgzfna
    pgzfnfrsmhhzwhEVERYymdziyyDAYfkrfduq
    rdfvdcscoBUTcWHENjYOUxWRITEtMEfuszck
    ohrhITSyOKAYecveaoqownfuuallqzkwdipx
    lyuwpqjIrnHOPEwYOUREeSAFEtblggdeurfs
    emzalojIcfHOPEcYOUREeWELLjovrjtjrejg
    IxpBETqjTHERESekTALEShrskkomwveyuhyt
    zsfntovnvifeocefilzYOUDjtoLOVErvxtaj
    jmaydejwazntjyincthysqyglvhmeTOiTELL
    abfhgkyodclrljwpzaokvtizfyvxcdiunksu
    hohTHErBOOMSvdngANDkhitaCRASHESoeoos
    scdujfemNEVERjCEASEjtajwlroxkqkbqhwv
    emfxuszbmWEbNEVERqibcguixuufiiomgchw
    kixxrdvGETuAcBITozueewOFhPEACEpicjih
    bfcfdhdzdkfzIDoLOVEvONCEbMOREljfnkms
    czoficuwujesgswkmmklamsjijvvgncojzpk
    rpdeqtxmbqbpTOyHEARwYOURihhwkasxbnih
    tstiztijutcljemqoxqltvfelVOICEkvjcap
    asmewFORaJUSTsqrefsowveorrpvtptkinnk
    sgAgMOMENTwvpnrclupofqjatnpvfnobdzgu
    jpwackuyvpbhzsvdxTHROUGHjTHEkqyrnsny
    wutvpidqmvcgceyfkwdmascrazkzrvekqqfl
    jfwcuopvugyczseudhsbdsekxxbdiwmesylc
    NOISEvychumspccubjdjbaeuipocvdpqniug