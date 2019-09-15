(function($) {
    $(document).ready(function() {
        $(".dropdown-menu").delegate("li", "click", function() {
            var str = JSON.stringify($(this).html());
            var addRegex = /Add/i
            var saveRegex = /Save/i
            var loadRegex = /Load/i

            var notFile = !(addRegex.test(str) || saveRegex.test(str));

            if (notFile) {
                $(this).addClass("active").siblings().removeClass("active");
            }

        });
        // FOR MENU COLOUR
        $('ul.dropdown-menu [data-toggle=dropdown]').on('click', function(event) {
            event.preventDefault();
            event.stopPropagation();
            $(this).parent().siblings().removeClass('open');
            $(this).parent().toggleClass('open');
        });
        // NEED FOR TABS
        $('.collapse').on('shown.bs.collapse', function() {
            $(this).parent().find(".glyphicon-plus-sign").removeClass("glyphicon-plus-sign").addClass("glyphicon-minus-sign");
        }).on('hidden.bs.collapse', function() {
            $(this).parent().find(".glyphicon-minus-sign").removeClass("glyphicon-minus-sign").addClass("glyphicon-plus-sign");
        });
        /// for asp toggle (dev mode)
        $("[name='my-checkbox']").bootstrapSwitch();
        $('input[name="my-checkbox"]').on('switchChange.bootstrapSwitch', function(event, state) {
            if (state) {
                viewModel.asp(viewModel.bigAsp);
            } else {
                viewModel.asp(viewModel.smallAsp);
            }
            viewModel.aspState = state;
        });
    });

    $(".results-button").each(function() {
        expanded = $(this).hasClass("expanded");
        if (expanded) {
            $(this).find(".fa-plus").hide();
            $(this).find(".fa-minus").show();
        } else {
            $(this).find(".fa-plus").show();
            $(this).find(".fa-minus").hide();
        }
    });

    $(".results-button").click(function() {
        console.log("clicked")
        expanded = $(this).hasClass("expanded");
        if (expanded) {
            $(this).removeClass("expanded");
            $(this).find(".fa-plus").show();
            $(this).find(".fa-minus").hide();
        } else {
            $(this).addClass("expanded");
            $(this).find(".fa-plus").hide();
            $(this).find(".fa-minus").show();
        }
    });

})(jQuery);