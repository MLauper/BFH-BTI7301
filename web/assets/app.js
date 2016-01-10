'use strict';

var thefuseproject;
(function (thefuseproject) {
    
    function AppViewModel($scope, $http, apiRootUrl) {
        var self = this;

        listDir("");
        listDir("#<STANDARD-CLASS THEFUSEPROJECT::SAMPLE-CLASS2>");
        listDir("#<STANDARD-CLASS THEFUSEPROJECT::SAMPLE-CLASS2>/#<THEFUSEPROJECT::SAMPLE-CLASS2 {1003CBDCD3}>");
        isDir  ("#<STANDARD-CLASS THEFUSEPROJECT::SAMPLE-CLASS2>");
        isDir  ("#<STANDARD-CLASS THEFUSEPROJECT::SAMPLE-CLASS2>/#<THEFUSEPROJECT::SAMPLE-CLASS2 {1003CBDCD3}>");
        isDir  ("#<STANDARD-CLASS THEFUSEPROJECT::SAMPLE-CLASS2>/#<THEFUSEPROJECT::SAMPLE-CLASS2 {1003CBDCD3}>/THEFUSEPROJECT::SOMEINTEGER");
        isDir  ("#<STANDARD-CLASS THEFUSEPROJECT::SAMPLE-CLASS2>/#<THEFUSEPROJECT::SAMPLE-CLASS2 {1003CBDCD3}>/THEFUSEPROJECT::SOMEFALSEBOOLEAN");
        isDir  ("#<STANDARD-CLASS THEFUSEPROJECT::SAMPLE-CLASS2>/#<THEFUSEPROJECT::SAMPLE-CLASS2 {1003CBDCD3}>/THEFUSEPROJECT::SOMEOBJECT");
        isDir  ("#<STANDARD-CLASS THEFUSEPROJECT::SAMPLE-CLASS2>/#<THEFUSEPROJECT::SAMPLE-CLASS2 {1003CBDCD3}>/THEFUSEPROJECT::SOMELIST");

        function listDir(path) {
            return $http({
                url: apiRootUrl + '/list-dir/' + thefuseproject.normalizePath(path),
                method: 'GET'
            })
            .then(thefuseproject.mapData);
        }
        
        function isDir(path) {
            return $http({
                url: apiRootUrl + '/is-dir/' + thefuseproject.normalizePath(path),
                method: 'GET',
                timeout: 200
            })
            .then(thefuseproject.mapData)
            .catch(function() { return false })
            .then(function(v) { console.log(path, v); });
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
        .constant("apiRootUrl", "http://benidev.collab.ch:8000/api")
        .controller('AppViewModel', AppViewModel)
        .factory('debounce', DebounceFactory)
        .run(function($rootScope, $rootElement, apiRootUrl) {
            $rootScope.$ignore = function() { return false };
            $rootScope.$today = moment().startOf('day').toDate();
            $rootScope.$tomorrow = moment().add(1, 'day').startOf('day').toDate();
            $rootScope.$yesterday = moment().add(-1, 'day').startOf('day').toDate();
            $rootScope.apiRootUrl = apiRootUrl;
        });

    thefuseproject.mapData = function (promise) {
        console.log(promise.data);
        
        return promise.data;  
    };
    
    thefuseproject.normalizePath = function (path) {
        if (!path) {
            path = "#";
        }
        
        return encodeURIComponent(path);
    };


})(thefuseproject || (thefuseproject = {}));