//Google Analytics
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
  a=s.createElement(o), m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-116223405-1', 'auto');
  ga('send', 'pageview');


// FRONT PAGE

// Track report plot type for perm exclusions

$(document).on('change', '#bars_type :radio', function(e) {
  if(this.checked && this.value == 'rate'){
      ga('send', 'event', 'widget', 'select plot type perm excl', $(e.currentTarget).val());
    } 
  else if(this.checked && this.value == 'number'){
      ga('send', 'event', 'widget', 'select plot type perm excl', $(e.currentTarget).val());
    }
  });

// Track report plot type for fixed exclusions radio buttons

$(document).on('change', '#bars_type2 :radio', function(e) {
  if(this.checked && this.value == 'rate'){
      ga('send', 'event', 'widget', 'front page / select plot type perm excl', $(e.currentTarget).val());
    } 
  else if(this.checked && this.value == 'number'){
      ga('send', 'event', 'widget', 'front page / select plot type perm excl', $(e.currentTarget).val());
    }
  });
  

// PUPIL CHARACTERISTICS TAB
  
  //Track measure of exclusion type selected
  $(document).on('change', '#char_cat', function(e) {
    ga('send', 'event', 'widget', 'characteristics / select exclusion measure', $(e.currentTarget).val());
  }); 
  
  //Track measure of school types selected
  $(document).on('change', '#char_sch', function(e) {
    ga('send', 'event', 'widget', 'characteristics / select school type', $(e.currentTarget).val());
  });
  
  //Track measure of pupil characteristics interested in 
  $(document).on('change', '#char_char', function(e) {
    ga('send', 'event', 'widget', 'characteristics / select pupil characteristic', $(e.currentTarget).val());
  });
  
  //Track downloads of pupil characteristic data
  $(document).on('click', '#download_characteristics_data', function() {
    ga('send', 'event', 'widget', 'characteristics / download data');
  });
  
// LOCAL AUTHORITY TAB

 //Track measure of exclusion type selected
  $(document).on('change', '#select2', function(e) {
    ga('send', 'event', 'widget', 'la / area selection', $(e.currentTarget).val());
  }); 
  
  //Track measure of exclusion type selected
  
  $(document).on('change', '#select_cat', function(e) {
    ga('send', 'event', 'widget', 'la / exclusion category', $(e.currentTarget).val());
  });
  
  // Track report plot type for la tab radio buttons

  $(document).on('change', '#plot_type :radio', function(e) {
    if(this.checked && this.value == 'rate'){
        ga('send', 'event', 'widget', 'la / rate', $(e.currentTarget).val());
      } 
    else if(this.checked && this.value == 'number'){
        ga('send', 'event', 'widget', 'la / number', $(e.currentTarget).val());
      }
    });
    
    //Track downloads of la trend data
    
  $(document).on('click', '#la_data_download_tab_1', function() {
    ga('send', 'event', 'widget', 'la / download trend data');
  });
  
  //Track downloads of la comparison to regional and national data
    
  $(document).on('click', '#la_data_download_tab_2', function() {
    ga('send', 'event', 'widget', 'la / download comparison to regional and national data');
  });
  
// MAP TAB

//Track measure of exclusion type selected for map

  $(document).on('change', '#select_map', function(e) {
    ga('send', 'event', 'widget', 'map / exclusion measure', $(e.currentTarget).val());
  }); 
  
// REASON FOR EXCLUSION TAB 












  //Track measure of area
  $(document).on('change', '#la_name_exclusion_select', function(e) {
    ga('send', 'event', 'widget', 'reason / select area level', $(e.currentTarget).val());
  }); 
  
  //Track measure of school types selected
  $(document).on('change', '#schtype', function(e) {
    ga('send', 'event', 'widget', 'reason / select school type', $(e.currentTarget).val());
  });
  
  //Track measure of exclusion category
  $(document).on('change', '#exclusion_type', function(e) {
    ga('send', 'event', 'widget', 'reason / select exclusion type', $(e.currentTarget).val());
  });
  
  //Track downloads of pupil characteristic data
  $(document).on('click', '#download_reason_for_exclusion', function() {
    ga('send', 'event', 'widget', 'reason / download data');
  });
  
  
// SCHOOL TYPE TAB

//Track measure of area
  $(document).on('change', '#la_name_rob', function(e) {
    ga('send', 'event', 'widget', 'school tab / select area level', $(e.currentTarget).val());
  }); 
  
  //Track measure of school name
  $(document).on('change', '#EstablishmentName_rob', function(e) {
    ga('send', 'event', 'widget', 'school tab / select school name', $(e.currentTarget).val());
  });

//Track downloads of school level data
  $(document).on('click', '#school_data_download', function() {
    ga('send', 'event', 'widget', 'school tab / download data');
  });
  
// DATA AND METHODS TAB

// 1. Top Download Button for Main UD
  $(document).on('click', '#downloadmain_ud', function() {
    ga('send', 'event', 'widget', 'data and methods / main underlying data');
  });
  
// 2. Top Download Button for reason UD
  $(document).on('click', '#downloadreason_ud', function() {
    ga('send', 'event', 'widget', 'data and methods / reason underlying data');
  });  
  
// 3. Top Download Button for characteristics UD
  $(document).on('click', '#downloadnatchar_ud', function() {
    ga('send', 'event', 'widget', 'data and methods / characteristics underlying data');
  });   











  
  
  
