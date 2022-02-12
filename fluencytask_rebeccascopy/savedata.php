<?php

    session_start();

    // Disable magic quotes
    //if (get_magic_quotes_gpc()) {
    //    $process = array(&$_GET, &$_POST, &$_COOKIE, &$_REQUEST);
    //    while (list($key, $val) = each($process)) {
    //        foreach ($val as $k => $v) {
    //            unset($process[$key][$k]);
    //            if (is_array($v)) {
    //                $process[$key][stripslashes($k)] = $v;
    //                $process[] = &$process[$key][stripslashes($k)];
    //            } else {
    //                $process[$key][stripslashes($k)] = stripslashes($v);
    //            }
    //        }
    //    }
    //    unset($process);
    //}

    $file = "S".$_SESSION['userid']."_data.txt";
    $file = fopen('./logs/'.$file, 'w');
    fwrite($file, $_POST['games_json']);
    fclose($file);    

    $file = "S".$_SESSION['userid']."_distractor.txt";
    $file = fopen('./logs/'.$file, 'w');
    fwrite($file, $_POST['distractor_json']);
    fclose($file);    
    
?>
