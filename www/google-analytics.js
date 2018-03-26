//Google Analytics
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
  a=s.createElement(o), m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-116223405-1', 'auto');
  ga('send', 'pageview');

//Track measure inputted on map tab 
  $(document).on('change', '#select_map', function(e) {
    ga('send', 'event', 'widget', 'measure selected in map tab', $(e.currentTarget).val());
  });
