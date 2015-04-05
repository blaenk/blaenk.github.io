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

  var toggleForm = function(button, form) {
    button.click(function() {
      if (form.is(':visible')) {
        form.fadeOut('fast');
      } else {
        form.fadeIn('fast');
        form.find('input').focus();
      }
    });

    form.find('input').keyup(function(e) {
      if (e.keyCode == 27) {
        form.fadeOut('fast');
      }
    });
  };

  toggleForm($('#search_btn'), $('.desk_search'));

  window.refresh = function () {
    // header links
    $('.entry-content').children('h1, h2, h3, h4, h5').each(function() {
      var $id = $(this).attr('id');
      var $text = $(this).text();

      $(this)
        .html('')
        .prepend('<span class="hash">#</span>')
        .append('<a href="#' + $id + '" class="header-link">' + $text + '</a>');
    });

    $('.collapse').not(function() {
      return "#" + $(this).attr('id') == window.location.hash;
    }).next('.collapsible').hide();

    $('.collapse').click(function(e) {
      e.preventDefault();
      $(this)
        .next('.collapsible')
        .slideToggle('fast');
    });

    var $footnotes = $('.footnotes > ol > li');

    $('.footnoteRef')
      .attr('title', 'read footnote')
      .click(function() {
        $footnotes.stop(true, true)

        var note = $(this).attr('href');
        $footnotes.not(note)
          .css({opacity: 0.1})
          .animate({opacity: 1.0}, 15000, 'linear');
      });

    $('a[href^="#fnref"]')
      .attr('title', 'continue reading')
      .html('<i class="fa fa-level-up"></i>')
      .click(function() {
        $footnotes.stop(true, true);
      });

  };

  window.refresh();
});
