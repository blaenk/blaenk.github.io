$(function() {
  var mobilenav = $('#mobile-nav');

  $('html').click(function(){
    mobilenav.find('.on').each(function(){
      $(this).removeClass('on').next().hide();
    });
  });

  mobilenav.on('click', '.menu .button', function(e){
    if (!$(this).hasClass('on')) {
      var width = $(this).width() + 42;
      $(this).addClass('on').next().css({width: width}).show();
    } else {
      $(this).removeClass('on').next().hide();
    }
  }).on('click', '.search .button', function(e){
    if (!$(this).hasClass('on')){
      var width = mobilenav.width() - 51;
      mobilenav.children('.menu').children().eq(0).removeClass('on').next().hide();
      $(this).addClass('on').next().show().css({width: width}).children().children().eq(0).focus();
    } else {
      $(this).removeClass('on').next().hide().children().children().eq(0).val('');
    }
  }).click(function(e){
    e.stopPropagation();
  });

  var $nav = $('#main-nav .main');
  var $search = $('.desk_search');

  var hideSearch = function() {
    $nav.removeClass('searching');
    $search.hide();
  };

  var showSearch = function() {
    $nav.addClass('searching');
    $search.show().find('input[type="text"]').focus();
  };

  $('#search_btn').click(function() {
    if ($nav.hasClass('searching')) {
      hideSearch();
    } else {
      showSearch();
    }
  });

  $('.desk_search input[type="text"]').keyup(function(e) {
    if (e.keyCode == 27) {
      hideSearch();
    }
  })

  window.refresh = function () {
    // header links
    $('.entry-content').find('h1, h2, h3, h4, h5').each(function() {
      var $id = $(this).attr('id');
      var $text = $(this).text();

      $(this).html('').prepend('<span class="hash">#</span>')
             .append('<a href="#' + $id + '" class="header-link">' + $text + '</a>');
    });

    $('.collapse').not(function() {
      return "#" + $(this).attr('id') == window.location.hash;
    }).next('.collapsible').hide();

    $('.collapse').click(function(e) {
      e.preventDefault();
      $(this).next('.collapsible').slideToggle('fast');
    });

    $('a[href^="#fnref"]').html('<i class="fa fa-level-up"></i>');
  };

  window.refresh();
});
