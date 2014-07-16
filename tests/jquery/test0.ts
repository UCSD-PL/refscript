///<reference path="./jquery.d.ts" />
"use strict";

function ex1(){

  //@ ts          :: {v:JQuery | size(Time, v) >= 1}
  var ts          = undefined;
  
  //@ cs          :: {v:JQuery | size(Content, v) >= 1}
  var cs          = undefined;
  
  //@ tsAndCs     :: {v:JQuery | size(Content, v) >= 1 && size(Time, v) >= 1}
  var tsAndCs     = ts.add(cs);
  
  //@ tsAndCs     :: {v:JQuery | size(Author, v) >= 1 }
  var prevTsAndCs = tsAndCs.prev();

}

//GOOD

1 /*::
2 (PlaysTable : TableElement ids = [playstable] classes = [plays]
3 (PlaysRow : TRElement classes = [playsrow]
4 (Title : TDElement classes = [title])
5 (Genre : TDElement classes = [genre])
6 (Year : TDElement classes = [year])
7 )+ );
8 */
9 $(’td’) // Has type t1 = jQueryh1+hTitlei++1+hGenrei++1+hYeari, AnyJQi
10 .filter(’:contains("henry")’) // t2 = jQueryh0+hTitle+Genre+Yeari, t1i
11 .ifMany(function() { // [jQueryh1+hTitle+Genre+Yeari, t1i] -> Undef
12 this // t3 = jQueryh1+hTitle+Genre+Yeari, t1i
13 .nextAll() // t4 = jQueryh1+hGenrei ++ 1+hYeari, t3i
14 .andSelf() // jQueryh1+hGenrei ++ 1+hYeari ++ 1+hTitlei, t4i
15 .addClass(’highlight’); // allowed, non-empty collection
16 });


//GOOD

1 /*::
2 (SampleDiv : DivElement ids = [jqdt2] classes = [samplediv]
3 (Paragraph : PElement classes = [goofy]
4 ...) // The children of a Paragraph are irrelevant here
5 (OrderedList : OLElement classes = [list]
6 (LinkItem : LIElement classes = [linkitem]
7 (Link : AElement classes = [link]))
8 (GoofyItem : LIElement classes = [goofy]
9 (StrongText : StrongElement classes = [strongtext]))
10 (FunnyItem : LIElement classes = [funny])
11 <LinkItem>
12 <GoofyItem>));
13 */
14 $(’li.goofy’) // Has type t1 = jQueryh1+hGoofyItemi, AnyJQi
15 .parents(’#jqdt2’) // t2 = jQueryh1+hSampleDivi, t1i
16 .children(’p’) // t3 = jQueryh1+hParagraphi, t2i
17 .next() // t4 = jQueryh1+hOrderedListi, t3i
18 .find(’a’) // t5 = jQueryh1+hLinki, t4i
19 .parent(); // t6 = jQueryh1+hLinkItemi, t5

//GOOD

1 /*::
2 (NewsTable : TableElement classes = [news] ids = [news]
3 ... // Placeholder element; never queried
4 (NewsBody : TBodyElement classes = [newsbody]
5 (YearRow : TDElement classes = [yearrow])
6 (NewsRow : TRElement classes = [newsrow] optional classes = [alt]
7 (NewsInfo : TDElement classes = [info])+)+
8 <YearRow>
9 <NewsRow>+))
10 */
11 $(’#news’) // Has type t1 = jQueryh1hNewsTablei, AnyJQi
12 .find(’tr.alt’) // t2 = jQueryh1+hNewsRowi, t1i
13 .ifMany(function() { this.removeClass(’alt’); }) // t2
14 .end() // t1
15 .find(’tbody’) // t3 = jQueryh1hNewsBodyi, t1i
16 .each(function() { // [NewsBody] -> Undef
17 $(this) // t4 = jQueryh1hNewsBodyi, AnyJQi
18 .children() // t5 = jQueryh1+hYearRowi ++ 1+hNewsRowi, t4i
19 .filter(’:visible’) // t6 = jQueryh0+hYearRowi++0+hNewsRowi, t5i
20 .has(’td’) // t7 = jQueryh0+hNewsRowi, t6i
21 .ifMany(function() { // [jQueryh1+hNewsRowi, t6i] -> Undef
22 this.addClass(’alt’); // allowed, non-empty collection
23 }); // t7
24 }); 

//BAD

1 $(’li.goofy’) // Has type t1 = jQueryh1+hGoofyItemi, AnyJQi
2 .parents(’#WRONG_ID’) // t2 = jQueryh01hElement @ "#WRONG_ID"i, t1i
3 .children(’p’);
4 ⇒ ERROR: children expects 1+hElementi, got 01hElementi
5 $(’#news’) // Has type t3 = jQueryh1hNewsTablei, AnyJQi
6 .find(’tr.alt’) // t4 = jQueryh1+hNewsRowi, t3i
7 .ifMany(function() { this.removeClass(’alt’); }) // t48 // Note: missing call to .end()
9 .find(’tbody’) // t5 = jQueryh0hElementi, t3i
10 .each(...)
11 ⇒ ERROR: each expects 1+hElementi, got 0hElementi
12 $(".tweet").children().next().next().next().css("color", "red");
13 ⇒ ERROR: css expects 1+hElementi, got 0hElementi
14 $(".tweet").children().next().css("color");
15 ⇒ ERROR: css expects 1hElementi, got 1+hAuthor + Timei


