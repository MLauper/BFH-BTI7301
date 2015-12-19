var thefuseproject;
(function (thefuseproject) {
	
	function ViewModel($scope, $state, $window, $location, $http) {
		var data = {
			assessmentId: 'hiii?'
		};
		$http({
			url: 'http://benidev.collab.ch:8000/api/list-directory/' + encodeURIComponent(""),
			method: 'GET',
			params: null,
			data: data,
		}).then(function (res) {
			$scope.result2 = res;
		});
		$http({
			url: 'http://benidev.collab.ch:8000/api/list-directory/' + encodeURIComponent("#<STANDARD-CLASS THEFUSEPROJECT::SAMPLE-CLASS2>"),
			method: 'GET',
			params: null,
			data: data,
		}).then(function (res) {
			$scope.result = res;
		});
	}
	
	angular.module('thefuseproject', ['ui.router', 'ui.bootstrap', 'ngAnimate'])
		.controller('IndexViewModel', ViewModel);
	
})(thefuseproject || (thefuseproject = {}));