# Mathematica Advent of Code
# 2022

This repository stores my [Advent Of Code](http://adventofcode.com/2022/) solutions for 2022, written in Mathematica.  Some of these solutions are simple one-liners, and others are procedural and not fundamentally different from most other common programming languages, and so don't have any READMEs associated with them.  Some (marked in **bold**) are more involved and have (or need) explanations.  I'm using .wls instead of .m, and trying to make the code somewhat readable as plaintext, but casual Mathematica in general is notebook-heavy, and isn't great to view on GitHub.

There's also a utilities file, Ulitities.nb, with some functions that proved useful in previous years.  This is not a true utils file, in that it isn't a paclet and that I don't import the function definitions directly, but it's mostly there as boilerplate code I can copy in when need be.

## Days Completed

* Day 1: Counting Calories
* Day 2: Rock Paper Scissors
* **Day 3: Rucksack Reorganization**: Includes GPT-3 generated Python code; GPT-3 was not able to generate Mathematica.
* Day 4: Camp Cleanup
* Day 5: Supply Stacks
* Day 6: Tuning Trouble
* Day 7: No Space Left On Device
* Day 8: Treetop Tree House
* Day 9: Rope Bridge
* Day 10: Cathode-Ray Tube
* Day 11: Monkey in the Middle
* Day 12: Hill Climbing Algorithm
* Day 13: Distress Signal
* Day 14: Regolith Reservoir
* Day 15: Beacon Exclusion Zone
* **Day 16: Proboscidea Volcanium**: Contains both optimized Mathematica solution and unoptimized Rust solution, which run at around the same speeds.  Rust solution is for part 2 only.
* Day 17: Pyroclastic Flow
* Day 18: Boiling Boulders
* **Day 19: Not Enough Minerals**: Like day 16, has optimized Mathematica and partially optimized Rust.
* Day 20: Grove Positioning System
* Day 21: Monkey Math
* Day 22: Monkey Map
* Day 23: Unstable Diffusion
* Day 24: Blizzard Basin
* Day 25: Full of Hot Air

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
    hqqqqtcccbzzzwzrrrdqdldpdsppmqmmnwwz
    zizzTHEmNOISEjjghyaonuykfvostyphwkqh
    tujuexqGETSzLOUDERvgwnoopollnmjiovdl
    szevzocnhtqrfvEVERYslgenrxDAYhicpdnm
    qochnbqghBUTaWHENhYOUeWRITEmMEmbxlwo
    wflmITSpOKAYrdkkrhjcohipabhuulkbsosr
    jpcaxdoInrHOPEzYOUREoSAFExmhqviimetr
    kgonjdtIirHOPEvYOUREeWELLjzxjeikhwjv
    IpwBETcxTHEREScnTALESsieuqitrpyayojh
    nnpkcavciqsitvwafvyYOUDmgjLOVEplcwhv
    jnwstaqrinfyfdzkoodphgcmxwcneTOcTELL
    acpbvfbimjzpzixxezxxevyyksmhxtcqbnlj
    fioTHEpBOOMSftpyANDrzhalCRASHESxnfjj
    xlpegyodNEVERxCEASEegffoeifugeutbewm
    wthheyvodWEwNEVERtirbvzjmmecqwogatbs
    aukfzbzGETkAgBITaocnzkOFvPEACEeyoryi
    jmonwiIhWISHfTHAThIfuxocrmxdxarwanxe
    mjazxjwxfycjvbgxwwhvlaknlmsigmoqwirl
    xvnjbpzugybkCOULDoHEARqYOURhomyxivwf
    iafsqivcljztmtdvfczzgcfqmVOICEwmqpii
    FOReJUSTjmwwujorbneysozjnxlzdygfnbnw
    qgamrsvlAmMOMENTtptqvtxevzauqnkfuukm
    xqysxqqvzqaihzbTHROUGHdacTHEvhgexnkx
    wszjyopxhtlpvophsngknwgbzafcwfkfoajr
    zizuauhokymoevcsnwzxtzhvkzevnoiseijn
    litcxkbkvrfakwyzuofzmxewuiwrdqcbmbay
    
### [Day 7](https://www.adventofcode.com/2022/day/7): Day Seven's Sonnet

Initialize the stack, and add the root  
(The root's the forward slash, just so you know).  
The `cd x` commands aren't absolute,  
So `cd x` goes up; `..`, below.  

And now you should initialize a hash  
(Not memory-efficient, but it's fine).  
You'll travel all the way back up to `/`  
Each time you see a number start a line.  

For every subdirectory you see,  
You'll add-assign the file size, and then  
You'll look for the next file (or `cd`)  
And when you find one, do it all again.  

You'll filter out by size to solve part 1,  
And part 2 works the same.  And now, you're done.  

### [Day 8](https://www.adventofcode.com/2022/day/8): [I Wonder Where the Flowers Is](https://www.barrypopik.com/index.php/new_york_city/entry/brooklyn_national_anthem_spring_is_sprung)

It's visible, the trees is,  
From way outside the grid,  
Unless the outside breezes  
(At ten times nine degrees-es)   
Hit taller trees than these is.  
If so, the trees is hid.  

A local's expertise is  
What gives the scenic score.  
She counts up what she sees-es:  
The short trees and their leaves-es.  
(The tall ones, hid by eaves-es).  
She multiplies the four.  

You might want our committee  
To not do what Therese says.  
The north side sees the city!  
The south side sees the seas-es!  
You might think it's a pity  
To follow these caprices,  
But I'll give you the keys-es  
To visit when you pleases,  
And though it's itty-bitty  
I'm certain you'll agrees-es:  
It's really rather pretty,  
This house atop the trees-es.  

### [Day 9](https://www.adventofcode.com/2022/day/9): Lazy Limerick #1

There's a treacherous bridge, and a strait  
But your rope physics models can't wait.  
As the bridge breaks apart  
Hope your model is smart;  
Better hurry, or you will be late.  

### [Day 10](https://www.adventofcode.com/2022/day/10): [One Shade The More, One Ray The Less](https://www.poetryfoundation.org/poems/43844/she-walks-in-beauty)

The CPU counts down the second  
To draw its pixels like a pen.  
Just fix the thing, and at your beck and  
Call the Elves will be again.  

(Then again, *you've* done the work here  
Getting gadgets up to spec.  
Perhaps you'll rest a moment, lurk here  
Before you're at their call, and beck).  

There is a forty cycle bookend  
Drawing pixels left to right.  
You'll know, if you check each nook and  
Cranny, where to put the sprite.  

(But man, [you've timed](https://adventofcode.com/2016/day/15) [a lot](https://adventofcode.com/2016/day/25) [of circuits](https://adventofcode.com/2017/day/1),  
And [given opcodes](https://adventofcode.com/2017/day/23) [lots](https://adventofcode.com/2016/day/12) [of looks](https://adventofcode.com/2018/day/19).  
Perhaps you'll rest; it's always work, it's  
Searching crannies and their nooks.)  

You've found a dry spot in this cove (or  
Dry enough to fix the fall) -  
"I'm on my way", you tell them, "Over  
And out", you say, and end the call.  

(You didn't give an ETA, no  
Promise for when you'd be back,  
And that's just fine; for all that they know  
It takes weeks.  You have some slack.  
And sure, it might take just a day, no  
Doubt you're skilled now as a rover,  
But sometimes rest is *mucho bueno*,  
Before you climb on out, and over.)  

### [Day 11](https://www.adventofcode.com/2022/day/11): Simian Shenanigans
*To the tune of the middle part of Weird Al's "[Hardware Store](https://youtu.be/DFI6cV9slfI?t=130)".*

Sigh.  
Would you look at all that stuff...  
They got:  

Sets of sandals, super-soakers,  
Stainless stacking sausage smokers,  
Stocking stuffers, sparking snuffers,  
Swimsuits and some snacks by Stouffers,  

[System-update space schematics](https://adventofcode.com/2022/day/7),  
[Signal strength sextuple statics](https://adventofcode.com/2022/day/10),  
Sulfide sputters, server stutters,  
Solid Shaker-style shutters,  

[Sonar sweeps](https://adventofcode.com/2021/day/1) and [systemed seating](https://adventofcode.com/2021/day/11),  
[Snailfishes](https://adventofcode.com/2021/day/18) and [see-through sheeting](https://adventofcode.com/2021/day/13),  
[Shuffle slammers](https://adventofcode.com/2019/day/13), [spinlock spammers](https://adventofcode.com/2017/day/17),  
[Submarine cetacean scrammers](https://adventofcode.com/2021/day/7),  

[Shakespeare sonnets](https://www.reddit.com/r/adventofcode/comments/e7pkmt/2019_day_8_solutions/fa2i13v/), [springdroid soarers](https://adventofcode.com/2019/day/21),  
[Santa's senseis](https://www.reddit.com/r/adventofcode/comments/z9he28/advent_of_code_2022_mistiltoe_elfucation/),  [syntax scorers](https://adventofcode.com/2021/day/10),  
[Sega slayers](https://www.reddit.com/r/adventofcode/comments/kgbylz/2020_day_119_solving_almost_all_puzzles_on_a_sega/), [site that's Slater's](https://www.reddit.com/r/adventofcode/comments/k1h4bq/2019_day_1_part_2_spits_out_right_answer_for/gdoyucm/)  
[Saturn Stoichiometrators](https://adventofcode.com/2019/day/14)!  

Man, that's a heavy pack!  You should have told me!  
No wonder that poor rope bridge couldn't hold me.  

### [Day 12](https://www.adventofcode.com/2022/day/12): [Excelsior!](https://www.poetryfoundation.org/poems/44631/excelsior-56d223cb4e6fa)

The shades of night were falling fast  
As through a lettered heightmap passed  
A programmer, who for advice  
Looked often at his strange device:  
Excelsior!  

He could not climb, but drops, he likes.  
Not monotonic were his hikes  
No straight path did he follow down  
But often checked, without a frown,  
Excelsior!  

He spiraled up the mountain's height  
And at the top, beheld a sight  
Of coders who had never stirred  
And who had never seen the word  
Excelsior!  

"Pray tell," said one to him who climbed  
"For us, the BFS was primed.  
We did not have to climb at all,  
So how'd you make it?  What's that called?"  
"Excelsior!"  

The answer came both quick and blunt.  
"It's just an advertising stunt!  
I'm representing Office Pro  
Who wanted everyone to know  
[Excelsior](https://www.reddit.com/r/adventofcode/comments/zkehl4/2022_day_12_time_for_some_excel_just_because_we/)!"  

### [Day 13](https://www.adventofcode.com/2022/day/13): Distress Signal

There's trouble here!  It's gone awry!  
We're out of cookies, cream, and pie!  
The ice cream truck, it passed us by!  
So do you copy?  Please reply!  

My hiking jacket ripped and tore  
(And also, there's a dinosaur  
That's smashing trees with quite the roar).  
We left the foosball on the shore!  

The eggnog's low - I think it's theft  
(It's huge, three tons at least, its heft)  
There's only fifteen barrels left,  
When they run dry, we'll be bereft!  

Our campsite is an awful mess -  
We lost the cleaning plans, I guess.  
(The monster looks like it's from Ness)  
So hear our signal of distress!  

### [Day 14](https://www.adventofcode.com/2022/day/14): The Castle

I took a bucket with me, and a trowel,  
Some sunscreen, an umbrella, not much more.  
I wore some swim trunks, had with me a towel,  
The day I made a sculpture on the shore.  

It took a bit of time; I didn't stop  
As waves crashed in and slunk back to the sea.  
I bucket-tipped, on tiptoes, overtop,  
Until the towers towered over me.  

A lofty goal a lot of work requires:  
I piled sand as tall as I could reach,  
And then I carved, made moats and keeps and spires,  
Until, at last, 'twas time to leave the beach.  

I ruled the tides and overlooked the land,  
The day I built a castle out of sand.  

### [Day 15](https://www.adventofcode.com/2022/day/15): Calculatus Eliminatus

It isn't underneath the sink,  
It isn't on the tables;  
I looked under the couch, I think,  
And even checked the cables.  

The basement, past those cobwebs?  No  
(Although I might have missed it).  
I couldn't move the washer, though;  
Perhaps you could assist it.  

The attic?  Eh - I saw a rat  
Last time I looked between.  
The attic is the one place that  
We never have to clean.  

Besides, there's no way it's in *there*  
If nobody's been in it,  
But two people could move these shelves  
If you could spare a minute.  

I peeled up all the rugs just now,  
And looked through all the keys.  
We've checked a lot of places - how  
Has it not been in these?  

In puzzlement, I scratch my head:  
Where have my glasses got?  
But I can't find them, so instead,  
I'll find out where they're not.  

### [Day 16](https://www.adventofcode.com/2022/day/16): Lazy Limerick #2

Get the flow rates from `AA` to `SQ`,  
And do not lay your head on your desk; you  
Might be six hours deep  
But who cares about sleep  
When there's elephants down there to rescue?  

### [Day 17](https://www.adventofcode.com/2022/day/17): Watch For Falling Rocks

Look out - the rocks are falling overhead!  
I *told* the elephants we should have fled!  
I tried to shove them, but they're mighty strong;  
And so we sit here, twiddling our trunks, instead.  

I tried to shove them, but they're mighty strong,  
Just *one*'s a pain, so try moving a throng!  
Though one of them helped calculate the flows,  
I sure wish I could move the rest along.  

Though one of them helped calculate the flows,  
And saved me a few minutes, I suppose,  
The rest are all scared stiff from some debris;  
How they got here at all, nobody knows.  

The rest are all scared stiff from some debris;  
And one has of them has a request for me:  
I need to find the cycle of the blocks:  
A trillion, piled up, how tall? asks he.  

I need to find the cycle of the blocks
And time their gas-jet patterns with my clocks,  
Eliminating rows the rocks can't reach,  
In short, I need to watch for falling rocks.  

Eliminating rows the rocks can't reach  
Saves lots of time, since there won't be a breach.  
Of course, I've also proved they won't reach *us*,  
But elephants don't like that little speech.  

Of course, I've also proved they won't reach us,  
And in so doing, demonstrated thus:  
If ever you can cordon off a height  
A cycle more, you'll cordon off 'height +`.  

If ever you can cordon off a height,  
Your well will shrink, and will remain finite.  
And finite wells, you keep within a hash  
(Assuming that you did the first steps right).  

And finite wells, you keep within a hash,  
And every new well, check against the cache  
And later (perhaps sooner) you will find  
A current state that's found within your stash.  

And later (perhaps sooner) you will find  
The formulae of cycle-finding kind.  
I need to find the cycle of the blocks  
To break this loop with which I've been confined.  

I need to find the cycle of the blocks  
And time their gas-jet patterns with my clocks,  
Eliminating rows the rocks can't reach,  
In short, I need to watch for falling rocks.  

### [Day 18](https://www.adventofcode.com/2022/day/18): Paindrops Keep Falling On My Head

I tried an umbrella;  
It now looks like Swiss.  
I ducked underwater,  
But felt the steam hiss.  
This rain is quite hot  
And there's something amiss  
When a hailstorm with lightning,  
Is safer than this.  

### [Day 19](https://www.adventofcode.com/2022/day/19): Round the Rugged Rocks

From deep within a twisting cave  
We walk into the light;  
I've joined the herd; both beast and nerd  
Have made it through the night.  

The falling rocks we dodged and weaved,  
The lava, we'd divert,  
And so we see the sun at last,  
Alive, intact, unhurt.  

From deep within the mountain's depths  
We walk into the light.  
The dawn is here, the smoke is clear,  
The sun is shining bright.  

The beacons signalling distress  
Have once again been found,  
Not one the more, not one the less,  
Not one left underground.  

From deep around the rugged rocks  
We walk into the light.  
We see the sky, the herd and I,  
We witness with delight.  

The stones beneath us shine, and wow!  
I do not have a doubt:  
The very rocks are treasures now,  
Now that we've made it out.  

### [Day 20](https://www.adventofcode.com/2022/day/20): The Court, The Camp, The Grove

The hour strikes nineteen, the day is twenty,  
For this, my daily entry in my log.  
My spirit's high, my legs are hurting plenty -  
A mountain's rather tough to take a jog.  
I know to millimeters (maybe centi-),  
Where I am, but don't have their travelogue.  
And so, in manner tried and true and tested,  
I don't know where I'm going, so I guessed it.  

This log has come in handy, I'll confess,  
Such as today, for this bit of decryption.  
But I don't write what's *useful*; I digress  
And write instead the fun parts, a description  
That's just enough to later uncompress -  
In other words, a *puzzle*, not transcription!  
And so, by sheerest chance, I wrote the key,  
That goes from `8-1-1` to `1-5-3`.  

It's way more fun like this, it's way less boring  
Than doing things the sensible and slow way.  
What fun's a hike if one is not exploring?  
The beaten path's surprises are DOA.  
When you're not dodging rocks and magma pouring,  
When you're not herding elephants, there's no way  
That you, when sitting safely, reminiscing,  
Could ever have imagined what you're missing.  

I got the list from my handheld device  
(A useful thing I kept in my supply kit).  
And mixed the list five times, and did that twice  
And got the answer just the way I like it.  
I could have taken all that good advice  
And wrote down where this star grove is - but strike it!  
Write down enough to make it fun, I say!  
And so concludes my entry for today.  

### [Day 21](https://www.adventofcode.com/2022/day/21): Lazy Limerick #3

Match the numbers the monkeys shout at you,  
And this time around, be grateful that you  
Do not need to get close  
And get fur up your nose  
Lest your allergies flare up and--ACHOO!  

### [Day 22](https://www.adventofcode.com/2022/day/22): The World Is Flat

"The world is flat!  The world is flat!"  
I heard a voice declare.  
I turned to see who spoke to me;  
A man was standing there.  

His clothes were ragged as his hair,  
His voice was loud and hoarse,  
He looked as if he'd hit a cliff:  
A grad student, of course.  

"The world is flat!" he cried again,  
With eyes bloodshot and red,  
And on a whim I turned to him  
And "No it's not," I said.  

He bent down and with shaking hands  
A painted map unrolled.  
When it unfurled, it showed the world  
In colors bright and bold.  

He pointed to the painted map  
And gestured with his toe.  
"Start here.  Face east.  Five steps, at least.  
Now, where'd you think you'll go?"  

I turned and stepped one pace, then two,  
But three was one too far.  
"You're off the edge, I do allege!  
So, where'd you think you are?"  

"The other side!"  I said to him,  
Upon that fateful day.  
I didn't think it *had* a brink  
So that's the only way.  

And then he asked - my heart is weak -  
"I mean, *exactly* where?  
"If I start *here*" - he gestured near -  
"Will I go *there*, or *there*?"  

The mathematics circled in  
Like vultures round the dying.  
I'd love to say I knew a way,  
But sadly, I'd be lying.  

"On endless planes", the student said,  
With just a gentle sigh,  
"You always know to start at `O`  
And go to `x` and `y`."  

"The distances and paths and trails  
Are trivial to tell.  
Old Euclid knew, and I know too,  
And now you know as well."  

"But if the world were *not* a plane",  
He asked the flat-Earth rube,  
"Could you create and calculate  
"The distance on a **cube**?"  

The world is flat!  The world is flat!  
That lesson's what I learnt.  
The world is flat, I'll tell you that:  
Imagine if it weren't!  

### [Day 23](https://www.adventofcode.com/2022/day/23): The Seeds

There's a seed that I took and I planted,  
Just a speck among many I found.  
Before that, I took plant life for granted,  
But that seed, sadly, never broke ground.  

Not discouraged, I planted another,  
In the soil that failed for the first,  
And that seed, like its late older brother,  
Never rooted, nor through the ground burst.  

And a third and a fourth failed together,  
Though I altered the soil they grew in.  
It was not until weeks of bad weather  
That the fifth broke the cycle of ruin.  

That fifth seed grew a leaf, but it wilted.  
Was it not enough water?  Too much?  
Then the stem began sagging.  It tilted  
Until I tied it up with a crutch.  

And I frantically researched the factors  
Of the soil and water and sun,  
I've fixed valves, pipes, and thermal reactors,  
But this fifth grew one leaf and was done.  

It took numerous trials, and error,  
It took varying soil and air,  
It took making light dimmer, or fairer,  
It took compost and potash and prayer.  

There were hundreds of seeds that I sowed there,  
There were fifty that ever took root.  
There were twenty-one sproutlings that showed there,  
And of those, just a dozen bore fruit.  

They were beautiful, yes, to see blooming,  
All those stars growing out of the earth.  
But I don't know, and I'm not presuming,  
What exactly the heartbreak was worth.  

When you're planting you're dealing with numbers,  
Since each seed grows alone and unguided.  
And attachment too early encumbers,  
And one gets sentimental, like I did.  

From their perilous humble beginning,  
They all glow like and grow to the sky.  
But though others would see this as winning,  
I'm too faint of heart for it.  Not I.  

### [Day 24](https://www.adventofcode.com/2022/day/24): Through the Wind and Snow

"Hurry, hasten,  
Put your face in  
To the basin!  

Litle twister  
Leaves a blister -  
Move it, mister!"  

"We evade it!  
Almost paid it,  
But we made it."  

"But my candy  
Would be dandy:  
Fetch it, Randy!"  

I retreated  
And completed,  
But I'll eat it,  

Either now or  
When I'm dour  
In an hour.  

### [Day 25](https://www.adventofcode.com/2022/day/25): A Ballade of Christmas

We set up camp along a jungle's shore,  
And play rock-paper-scissors for the snacks.  
Chat-GPT wins problems three and four,  
But can't quite parse and organize the stacks.  
Devices left with us go out of whacks  
(Excuse the extra `s`; I blame the trees)  
The rope bridge falls!  The cathode-ray tube cracks!  
At least in this hot jungle we won't freeze.  

Some scrounging simians steal scores of stuff,  
And then, we climb a cliffside and a hill.  
A signal for distress says "Things are tough!  
And all this sand just makes it tougher still!"  
We find a missing beacon with some skill,  
And teaching valves to elephants?  A breeze!  
The rocks and magma falling down could kill -  
At least in this volcano we won't freeze.  

The rocks are lovely, crystals wrapped in shells;  
We grab a few while we decode the plan,  
And then find numbers that a monkey yells:  
It turns out that the *real* monkey is man.  
The cube's complex, and vastly harder than  
The blizzard (though we should have brought some skis).  
We scan the grove, plant seedlings where we scan,  
And, last, solve this SNAFU so we won't freeze.  

ENVOI

Prince, these many Advents now, I've been a fan:  
It's been a joy to solve and work with these.  
I hope you too find joy in all you can,  
For He is born - enjoy the world He frees!  