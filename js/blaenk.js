$(function() {
	// Open external links in new window
	var externalLinks = function(){
		var host = location.host;

		$('body').on('click', 'a', function(e){
			var href = this.href,
				link = href.replace(/https?:\/\/([^\/]+)(.*)/, '$1');

			if (link != '' && link != host && !$(this).hasClass('fancybox')){
				window.open(href);
				e.preventDefault();
			}
		});
	};

	// Append caption after pictures
	var appendCaption = function(){
		$('.entry-content').each(function(i){
			var _i = i;
			$(this).find('img').each(function(){
				var alt = this.alt;

				if (alt != ''){
					$(this).after('<span class="caption">'+alt+'</span>');
				}

				$(this).wrap('<a href="'+this.src+'" title="'+alt+'"/>');
			});
		});
	};

	externalLinks(); // Delete or comment this line to disable opening external links in new window
	appendCaption(); // Delete or comment this line to disable caption

	var mobilenav = $('#mobile-nav');

	$('html').click(function(){
		mobilenav.find('.on').each(function(){
			$(this).removeClass('on').next().hide();
		});
	});

	mobilenav.on('click', '.menu .button', function(){
		if (!$(this).hasClass('on')){
			var width = $(this).width() + 42;
			$(this).addClass('on').next().show().css({width: width});
		} else {
			$(this).removeClass('on').next().hide();
		}
	}).on('click', '.search .button', function(){
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

	//$("[rel=footnote]").inlineFootnote();

	/*$('.desk_search input[type="text"]').blur(function() {
		var $nav = $('#main-nav .main');
		var $search = $('.desk_search');

		if ($(this).val() == "") {
			$nav.removeClass('searching');
			$search.hide();
			$nav.data('blurred', 1);
		}
	});*/

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

	// header links
	$('.entry-content').find('h1, h2, h3, h4, h5').each(function() {
		var $id = $(this).attr('id');
		var $text = $(this).text();

		$(this).html('').prepend('<span class="hash">#</span>').append('<a href="#' + $id + '">' + $text + '</a>');
	});

	$('.collapse').not(function() {
		return "#" + $(this).attr('id') == window.location.hash;
	}).next('.collapsible').hide();

	$('.collapse').click(function(e) {
		e.preventDefault();
		$(this).next('.collapsible').slideToggle('fast');
	});
});
