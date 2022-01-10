$(document).ready(function () {

    // jQuery transit transition speed
    $.fx.speeds._default = 750;

    // Rivets formatters
    rivets.formatters.length = function (value) {
        return value.length;
    }

    rivets.formatters.toMinutes = function (value) {
        mins = parseInt(value / 60)
        seconds = parseInt(value % 60);
        seconds = seconds < 10 ? "0" + seconds : seconds;
        return mins + ":" + seconds;
    }
    // variables for each game
    function gameObj() {
        this.gamenum = 0;
        this.init = function () {
            this.items = [];
            this.times = [];
            this.firsttime = [];
            this.category = "";
            this.starttime = 0;
            this.countdown = timeperlist;

        }
        this.init();
    }

    // variables for distractor
    function distObj() {
        this.distnum = 0;
        this.init = function () {
            this.solution = [];
            this.times = [];
            this.firsttime = [];
            this.problem = "";
            this.starttime = 0;
            this.countdown = timeperlist;

        }
        this.init();
    }

    function shuffle(o) {
        for (var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
        return o;
    }

    // Generate order of categories
    function genList(cat, numx) {
        invalid_list = 1;
        while (invalid_list) {
            invalid_list = 0;
            list = [];
            for (i = 0; i < 3; i++) {
                list.push(cat.slice());
                shuffle(list[i]);
            }
            list = $.map(list, function (n) { return n; }); // Flatten list
            for (i = 1; i < list.length; i++) {
                if (list[i] == list[i - 1]) {
                    invalid_list = 1;
                }
            }
        }
        return list;
    }


    var games = [];                                       // Store game results
    var categories = ["Clothing Articles", "Cities", "Countries", "Fruits", "Animals", "Methods of Transportation", "Toys", "Sporting Games", "Kitchen Utensils", "Musical Instruments", "Camping Equipment", "Vegetables", "Furniture"];   // Categories to use
    var numx = 2;                                         // How many times to do each list
    var tokens = [1, 2, 3, 4, 5, 3, 2, 1, 6, 5, 4, 6, 7, 8, 9, 10, 11, 9, 8, 7, 12, 11, 10, 12];
    var timeperlist = 10;                                // 90 minutes per list
    var list = genList(categories, numx);                  // Generate a valid list
    var equations = ["(2 + 2) ÷ 2", "(7-1) x 2", "10 ÷ 2 - 5", "6 + 6 + 6", "5 x 5 x 5", "100 x 10 ÷ 100", "20 - 100", "2 ÷ 2 x 2", "1 + 6", "36 ÷ 6 ÷ 6", "(34 - 4) ÷ 2", "10000 ÷ 10"];
    var game = new gameObj();                             // Keeps track of current game
    var distractor = new distObj();
    var firstkey = 1;

    rivets.bind($('body'), { game: game });

    // Press start
    $(".start").click(startGame);

    // Add item to list
    $("#current").keydown(function (e) {
        if (e.which == 9) {                              // Prevents "tab+enter" bug
            e.preventDefault();
        }
        if (e.keyCode == 13) {                           // If user hits enter:
            firstkey = 1;
            str = $(this).val();
            if ($.trim(str).length > 0) {
                game.items.push($.trim(str));            // store in list
                game.times.push((new Date).getTime());   // & store timestamp
                $(this).val("");                         // & clear input field
                $("#items").append("<p>" + str + "recorded</p>");
                $("#items p").fadeOut(800);
            }
        }
        if ((e.keyCode != 13) & (firstkey == 1)) {
            game.firsttime.push((new Date).getTime());
            firstkey = 0;
        }
    });

    // Add Problem to list
    $("#dist_current").keydown(function (e) {
        if (e.which == 9) {                              // Prevents "tab+enter" bug
            e.preventDefault();
        }
        if (e.keyCode == 13) {                           // If user hits enter:
            firstkey = 1;
            str = $(this).val();
            if ($.trim(str).length > 0) {
                distractor.problem.push($.trim(str));            // store in list
                $(this).val("");                         // & clear input field
                $("#problems").append("<p>" + str + "recorded</p>");
                $("#items p").fadeOut(800);
            }
        }
        if ((e.keyCode != 13) & (firstkey == 1)) {
            game.firsttime.push((new Date).getTime());
            firstkey = 0;
        }
    });

    function startTimer() {
        var timer = setInterval(function () {
            game.countdown--;
            if (game.countdown == 0) {
                clearInterval(timer);
                endGame();
            }
        }, 1000);
    }

    function startGame() {
        game.gamenum++;
        game.init();
        game.category = categories[tokens[game.gamenum - 1] - 1];
        game.starttime = new Date().getTime();

        $(this).parent().transition({ left: '-200%' }, function () {
            $(this).css({ left: '100%' });
        });
        $("#game").transition({ left: '0%' });
        $("#current").focus();
        startTimer();
    }

    function startDistractor() {
        distractor.distnum++;
        distractor.init();
        distractor.problem = equations[distractor.distnum - 1];

        $(this).parent().transition({ left: '-200%' }, function () {
            $(this).css({ left: '100%' });
        });
        $("#dist_trial").transition({ left: '100%' });
        $("#current").focus();
    }


    function endGame() {
        firstkey = 1
        var gamecopy = $.extend(true, {}, game);
        games.push(gamecopy);

        $("#current").val("");
        $("#game").transition({ left: '-200%' }, function () {
            $("#game").css({ left: '100%' });
        });

        if (game.items.length <= 5) {
            $("#too_few").transition({ left: '0%' });
            game.gamenum;   //hack to replay same round, because startGame() will increment gamenum
        } else {
            if (game.gamenum < (categories.length * numx)) {
                $("#between_categories").transition({ left: '0%' });
            } else {
                $("#endgame").transition({ left: '0%' });
                $.post("savedata.php", { json: JSON.stringify(games) });
            }
        }
    }

    function startDistractor() {
        distractor.distnum++;
        distractor.init();
        distractor.problem = equations[distractor.distnum - 1];

        $(this).parent().transition({ left: '-200%' }, function () {
            $(this).css({ left: '100%' });
        });
        $("#dist_trial").transition({ left: '100%' });
        $("#current").focus();
    }
    function endDist() {
        firstkey = 1
        var distcopy = $.extend(true, {}, distractor);
        distractor.push(distcopy);

        $("#current").val("");
        $("#dist_trial").transition({ left: '-200%' }, function () {
            $("#dist_trial").css({ left: '100%' });
        });
   
    }

    //function startDistractor() {
    //    $("#between_categories").parent().transition({ left: '-200%' }, function () {
    //    $("#distractor").transition({ left: '0' });    
    //    }
    //        $("#distractor").transition({ left: '-200' });



});
