'use strict';

var thefuseproject;
(function (thefuseproject) {
    
    function AppViewModel($scope, $http, rootUrl) {
        var self = this;

        function getAllLanguages() {
            return $http({
                url: rootUrl + '/controller.php?controller=user&action=languages',
                method: 'GET'
            })
            .then(thefuseproject.mapData);
        }
    }
    
    function DebounceFactory($timeout) {
        return function(callback, interval) {
            var timeout = null;
            return function() {
                var args = arguments;
                $timeout.cancel(timeout);
                timeout = $timeout(
                    function () { callback.apply(this, args); }, 
                    interval
                );
            };
        }; 
    }
    
    angular.module('thefuseproject', ['ui.bootstrap', 'ngAnimate'])
        .constant("rootUrl", "http://benidev.collab.ch:8000/api/")
        .controller('AppViewModel', AppViewModel)
        .factory('debounce', DebounceFactory)
        .run(function($rootScope, $rootElement, rootUrl) {
            $rootScope.$ignore = function() { return false };
            $rootScope.$today = moment().startOf('day').toDate();
            $rootScope.$tomorrow = moment().add(1, 'day').startOf('day').toDate();
            $rootScope.$yesterday = moment().add(-1, 'day').startOf('day').toDate();
            $rootScope.rootUrl = rootUrl;
        });

    thefuseproject.mapData = function (promise) {
        return promise.data;  
    };

})(thefuseproject || (thefuseproject = {}));