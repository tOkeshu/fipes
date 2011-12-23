/**
 * Main Fipes JS namespace
 */
window.App = {
    Models : {},
    Views  : {}
};

App.Router = Backbone.Router.extend({
    routes: {
        "/"            : "root",
        "/fipes"       : "fipes",
        "/fipes/:fipe" : "fipe"
    },

    root: function() {
        App.Routes.navigate('/fipes', true);
    },

    fipes: function() {
        App.FipeView = new App.Views.Fipe({el: $('#main')});
    },

    fipe: function(fid) {
        var fipe = new App.Models.Fipe({id: fid});
        App.Fipe = fipe;
    }
});

/**
 * Helpers. Highly dangerous stuff.
 */
App.Helpers = {};

/**
 * Misc. behaviours, such as global animations, routing...
 */
$(document).ready(function() {
    Backbone.sync = Backbone.TNetStrings.sync;

    App.Routes = new App.Router;

    if (!Backbone.history.start()) {
        App.Routes.navigate('/', true);
    }
});

