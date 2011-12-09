/**
 * Main Fipes JS namespace
 */
window.App = {
};

App.Router = Backbone.Router.extend({
    routes: {
        "/"      : "root",
        "/fipes" : "fipes"
    },

    root: function() {
        App.Routes.navigate('/fipes', true);
    },

    fipes: function() {
        console.log('hello fipes!');
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
    App.Routes = new App.Router;

    if (!Backbone.history.start()) {
        App.Routes.navigate('/', true);
    }
});
