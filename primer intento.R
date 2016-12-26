r <- POST("https://en.wikipedia.org/w/index.php?title=Special:Export", 
            +           body = "&pages=Pope&offset=2001-12-05T12:04:10Z&limit=100&action=submit")
stop_for_status(r)
content(r, "parsed", "application/xml")