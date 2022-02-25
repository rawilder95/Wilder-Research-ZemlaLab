<?php
    session_start();
?>

<!DOCTYPE html>
<html>
<head>
    <title>Naming Task</title>
    <link rel="stylesheet" type="text/css" href="bootstrap.min.css">
    <link rel="stylesheet" type="text/css" href="main.css">
    <script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js" defer></script>
    <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/rivets/0.8.0/rivets.bundled.min.js" defer></script>
    <script type="text/javascript" src="jquery.transit.min.js" defer></script>
    <script type="text/javascript" defer>
       var subj_id = <?php echo $_SESSION['userid']; ?>;
    </script>
    <script type="text/javascript" src="app.js" defer></script>
</head>

<body>

    <!--[if lte IE 8]>
    <p class="bg-danger">This website was not designed for Internet Explorer 8 or below.
                             Please upgrade your browser before continuing.</p>
    <![endif]-->

    

    <div id="instructions">
        <h1>Instructions</h1>
        For each round in this task, you will be given a category and asked to list as many
        items from that category as you can. For instance, if the category is
        instruments, you should list as many instruments as you can recall.
        <p>
        Don't overthink it - there is no penalty for wrong answers.
        <ul> 
            <li> You will have 90 seconds to list as many items from the category as possible.
            <li> Please rely only on memory alone.
            <li>
                Do not enter the same item twice in a round, or list two items with the same suffix.<br>
                For example if you list "guitar", do not list "guitars" in the same round.
        </ul>
        You will be asked to do this 24 times in total.
        <br><br>
        <a class="start" href="#">Click here to begin</a>
    </div>

    <div id="game">
        <div id="game_top">
            <div id="game_left">
                <h2>Name {game.category}</h2>
                <input id="current" type="text"
                       rv-enabled="game.countdown">
                <br>
                <span class="subtext">Press enter to submit after each item</span>
            </div>
            <div id="timer">{game.countdown | toMinutes}</div>
        </div>
        <div id="items">{game.items | length} {game.category} named</div>
    </div>



    <div id="between_categories">
        <h1>Time's up!</h1>
        You named { game.items | length } {game.category}.<br><br>

        You have completed { game.gamenum } of 24 rounds.<br><br>

        You will now have 30 seconds to count backwards from one number by another number.  <br><br>
        Don't worry about making a mistake and having the remainder of your answers be off. Just continue counting backwards from that number onwards.  <br> <br>   
        <a class="next_dist" href="#">Start Numbers Task</a>
    </div>


    <div id="between_distractors">
        <h1>Time's up!</h1>

        You counted {distractor.items | length} numbers. <br><br>
        The next round will involve listing items again for a category.  This may or may not be a category you have already done.
        If it is a category you've already done, it's OK to list items you've
        listed before, but you may also list new items. Please try to list as
        many items as possible, but only list an item once per round.<br><br>

        <a class="start" href="#">Continue to Next Task</a>
    </div>

    <div id="dist_trial">

        <div id="dist_top">
            <div id="dist_left">
                <h2>Count backwards from {distractor.counter} by {distractor.problem}'s </h2>
                <input id="dist_current" type="text"
                       rv-enabled="distractor.countdown">
                <br>
                <span class="subtext">Press enter to submit after each item</span>
            </div>
            <div id="dist_timer">{distractor.countdown | toMinutes}</div>
        </div>
        <div id="dist_items">{distractor.items | length} {distractor.solutions} named</div>
    </div>
    </div>


    

    <div id="too_few">
        <h1>Uh-oh...</h1>
        You only entered { game.items | length } {game.category}!<br><br>

        Please use the entire allotted time to try
        and generate as many items as possible, listing each item once per round.

     
        <a class="next_dist" href="#">Start next round</a>
    </div>

    <div id="endgame">
        <h1>Time's up!</h1>
        You named { game.items | length } {game.category}.

        <div id="postExptSurvey">
            <form action="demographics.php" method="post">
                <h3 id="surveyTitle">Post-Experiment Survey</h3>
                <span class="subtext">Please complete this survey. NOTE: This study is not complete until filling out this survey.</span>
                <ul>
                    <li>
                        Gender<br>
                        <input type="radio" name="gender" value="M"> Male
                        <input type="radio" name="gender" value="F"> Female
                        <input type="radio" name="gender" value="O"> Other
                        <input type="text" name="genderOther" size="15" maxlength="40" id="genderTxt">
                    </li>
                    <br>
                    <li>
                    
                        Age
                        <input type="text" name="age" size="3" maxlength="3" id="ageTxt" />
                    </li>
                    <br>
                    
                    <li>
                        Are you experiencing medical condition that could have impacted your memory, such as a recent concussion?<br>
                        <input type="radio" name="MedicalCondition" value="N"> No
                        <input type="radio" name="MedicalCondition" value="Y"> Yes (Please Specify)
                        
                        <input type="text" name="MedicalOther" size="15" maxlength="40" id="MedicalTxt">
                    </li>
                    <!--For the next two questions, please be honest. You will be paid regardless of your response.-->
                    <br>
                    <br>
                    <li>
                        <label for="try">Did you try to do the task?</label>
                        <input type="radio" name="try" value="Y"> Yes
                        <input type="radio" name="try" value="N"> No
                    </li>
                    <br><br>
                    <li> At any point did you cheat in the task? <br>
                    For example, looking up answers on the internet counts as cheating as it does not rely on memory alone
                    <input type="radio" name="cheating" value="Y"> Yes
                    <input type="radio" name="cheating" value="N"> No
                    </li>
                    <br>
                    <br>
                    <li>
                        Comments (optional):
                        <br />If you were confused, please tell us why you were confused here.<br />
                        <textarea name="comments" cols="40" rows="8" wrap="hard"> </textarea>
                    </li>
                </ul>
                <input type="submit" value="Submit to complete the experiment">
            </form>
        </div>
    </div>

</body>

</html>
