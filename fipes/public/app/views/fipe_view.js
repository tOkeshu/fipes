(function() {

    App.Views.Fipe = Backbone.View.extend({
        events: {
            'change input[type="file"]': 'redirect'
        },

        redirect: function() {
            var fipe = new App.Models.Fipe;
            fipe.save({}, {
                success: function() {
                    App.Routes.navigate(fipe.url(), true);
                }
            });
        }
    });
})();
