<?php
    session_start();
    $_SESSION['userid'] = $_POST['userid'];
    echo $_SESSION['userid'];
?>


<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8" />
    <title>THIS IS CONSENT FORM PLEASE</title>
    <link rel="stylesheet" type="text/css" href="main.css">


</head>
<body>
    <h1> <b>Informed Consent</b> </h1>

    <p><h2> <b>Study Title:</b> Evaluating Episodic Reliance During Semantic Retrieval</h2></p>
    <p><b>Principal Investigator:</b> Jeffrey Zemla, Assistant Professor </p>
    <p><b>Department and Institution:</b> Psychology, Syracuse University </p>
    <p><b>Compensation:</b> For your participation you will receive 0.5 SONA course credits for a 30 minute study or 1 course credit for a 60 minute study. </p>
    <p>
        <b>Description: </b>
        You   are   being   invited   to   participate   in   an   experiment   that   will   help   us
        understand how memory works. Your participation is voluntary and you may withdraw from
        participation at any time without penalty.
        In this study, you will be asked to perform smemory tasks. These tasks include listing items from various categories and solving analytical problems.

    <p> Your confidentiality will be maintained, as your data will not be associated with any personally identifiable information (e.g., names, phone
    numbers,   or   e-mail   addresses).   Your   responses   may   be   made   publicly   available   for
    research purposes, but your identity will remain anonymous. There are no known risks (neither physical nor emotional) associated with participating in this study.</p>

    <b>Contact: </b> If you have any questions, concerns, or comments regarding this research please contact
    <p>
        <b> Principle Investigator</b> <br />
        Dr. Jeffrey Zemla, Ph.D. <br />
        Department of Psychology <br />
        Syracuse University <br />
        jczemla@syr.edu <br />
    </p>


    <p>
        <b>Graduate Student Researcher </b> <br />
        Rebecca Wilder <br />
        Department of Psychology <br />
        Syracuse University <br />
        rawilder@syr.edu <br />
        </p>


        <a class="consent" href="main.php">CONSENT</a>


</body>
</html>
