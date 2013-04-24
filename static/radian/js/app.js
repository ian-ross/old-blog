'use strict';

function EgCtrl(plotLib, $http, $scope, $location, $timeout) {
  plotLib.midMonths = function(ms, y) {
    return ms.map(function(m) { return new Date(y, m, 15); });
  };

  $scope.$watch('$location.hash', function() {
    var url = "http://" + location.host + "/radian/eg/" +
      location.hash.slice(2) + ".html";
    $timeout(function() {
      $http.get(url).success(function(res) {
        res = res.replace(/<h3>(.|\n)*<\/h3>\n\n/m, "");
        $('div#main-container pre.include-source').remove();
        var ctr = $('div#main-container');
        ctr.append('<pre class="include-source">' +
                   '<code class="html"></code></pre>');
        var code = $($(ctr.children()[ctr.children().length-1]).children()[0]);
        code.text(res);
        code.highlight();
      }, 0);
    });
  });
}
EgCtrl.$inject = ['plotLib', '$http', '$scope', '$location', '$timeout'];

var negs = 28;
var egtitles = [ "Basic plot; CSV data", // 1
                 "Basic plot; JSON data", // 2
                 "Interactive legend; stroke fading", // 3
                 "X-axis zoom", // 4
                 "Stroke fade UI", // 5
                 "Stroke colour selection UI", // 6
                 "Date handling", // 7
                 "Data aggregation", // 8
                 "Bar charts", // 9
                 "Functional plots #1", // 10
                 "Functional plots #2", // 11
                 "Expression vectorisation", // 12
                 "Data binding", // 13
                 "<plot-options> directive", // 14
                 "Another <plot-options> example", // 15
                 "Basic points plot", // 16
                 "Log axes", // 17
                 "Second axes", // 18
                 "Bar chart", // 19
                 "Basic palette", // 20
                 "Discrete palette", // 21
                 "Discrete palette (mark)", // 22
                 "Functional + palette", // 23
                 "Histogram", // 24
                 "Banded palettes", // 25
                 "Data access via URL", // 26
                 "Area plot", // 27
                 "More functional palettes" ]; // 28

angular.module('myApp', ['radian']).
  config(['$routeProvider', function($routeProvider) {
    for (var eg = 1; eg <= negs; ++eg) {
      var n = (eg < 10 ? '0' : '') + eg;
      $routeProvider.when('/' + n, { templateUrl: 'eg/' + n + '.html',
                                     controller: EgCtrl });
    }
    $routeProvider.otherwise({ redirectTo: '/01' });
  }]).
  controller('BaseController', ['$rootScope', function($rootScope) {
    $rootScope.egs = [];
    for (var eg = 1; eg <= negs; ++eg) {
      var n = (eg < 10 ? '0' : '') + eg;
      $rootScope.egs.push({ link: "#/" + n, title: egtitles[eg - 1] });
    }
  }]);
