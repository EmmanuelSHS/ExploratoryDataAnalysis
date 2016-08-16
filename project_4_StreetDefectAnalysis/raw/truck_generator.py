#!/usr/bin/env python
# coding=utf-8

import random
import csv


HEADER = """
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Traffic and Complaints</title>
    <style>
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
      }
      #map {
        height: 100%;
      }
#floating-panel {
  position: absolute;
  top: 10px;
  left: 25%;
  z-index: 5;
  background-color: #fff;
  padding: 5px;
  border: 1px solid #999;
  text-align: center;
  font-family: 'Roboto','sans-serif';
  line-height: 30px;
  padding-left: 10px;
}

      #floating-panel {
        background-color: #fff;
        border: 1px solid #999;
        left: 25%;
        padding: 5px;
        position: absolute;
        top: 10px;
        z-index: 5;
      }
    </style>
  </head>

  <body>
    <div id="floating-panel">
      <button onclick="toggleHeatmap()">Toggle Heatmap</button>
      <button onclick="changeGradient()">Change gradient</button>
      <button onclick="changeRadius()">Change radius</button>
      <button onclick="changeOpacity()">Change opacity</button>
    </div>
    <div id="map"></div>
    <script>

var map, heatmap;

function initMap() {
  map = new google.maps.Map(document.getElementById('map'), {
    zoom: 11,
    center: {lat: 40.7, lng: -74.04},
  });

  heatmap = new google.maps.visualization.HeatmapLayer({
    data: getPoints(),
    map: map
  });\n
"""

HEATMAPHEAD = """
function toggleHeatmap() {
  heatmap.setMap(heatmap.getMap() ? null : map);
}

function changeGradient() {
  var gradient = [
    'rgba(0, 255, 255, 0)',
    'rgba(0, 255, 255, 1)',
    'rgba(0, 191, 255, 1)',
    'rgba(0, 127, 255, 1)',
    'rgba(0, 63, 255, 1)',
    'rgba(0, 0, 255, 1)',
    'rgba(0, 0, 223, 1)',
    'rgba(0, 0, 191, 1)',
    'rgba(0, 0, 159, 1)',
    'rgba(0, 0, 127, 1)',
    'rgba(63, 0, 91, 1)',
    'rgba(127, 0, 63, 1)',
    'rgba(191, 0, 31, 1)',
    'rgba(255, 0, 0, 1)'
  ]
  heatmap.set('gradient', heatmap.get('gradient') ? null : gradient);
}

function changeRadius() {
  heatmap.set('radius', heatmap.get('radius') ? null : 20);
}

function changeOpacity() {
  heatmap.set('opacity', heatmap.get('opacity') ? null : 0.2);
}

// Heatmap data: 500 Points
"""

END = """
</script>
    <script async defer
            src="https://maps.googleapis.com/maps/api/js?key=AIzaSyAFnMgbm4alWk3dFQgyWeVUTTK7G-abOig&libraries=visualization&callback=initMap">
</script>
</body>
</html>"""

LCORD = "corr"

LOBJ = "traffic"

POBJ = "pole"
COBJ = "cave"

line = """  var %s = [
              {lat: %s, lng: %s},
              {lat: %s, lng: %s}
  ];\n
  """

lobj = """
  var %s = new google.maps.Polyline({
              path: %s,
              geodesic: true,
              strokeColor: '#FF0000',
              strokeOpacity: 1.0,
              strokeWeight: 2
  });\n
"""

objset = """  %s.setMap(map);\n"""

cir = """
    var %s = new google.maps.Circle({
          strokeColor: '#0000FF',
          strokeOpacity: 0.5,
          strokeWeight: 2,
          fillColor: '#0000FF',
          fillOpacity: 0.2,
          map: map,
          center: {lat: %s, lng: %s},
          radius: 10
    });
"""

def getPoly(traffic):
    poly = ""  

    for i, x in enumerate(traffic):
            lcvName = LCORD+str(i)
            lobName = LOBJ+str(i)
            
            poly += line % (lcvName, x[2], x[1], x[4], x[3])
            poly += lobj % (lobName, lcvName)
            poly += objset % (lobName)

    return poly

def getCir(traffic):
    cirle = ""
    obj = COBJ if traffic[1][4] == 'Cave-in' else POBJ
    for i, x in enumerate(traffic):
            if i == 0: continue
            cirle += cir % (obj+str(i), x[6], x[7])
    return cirle

def generator(poth, cave, truck):
    Total = HEADER

    # traffic data
    Total += getPoly(truck)
    #Total += "}"


    Total += getCir(poth)
    Total += getCir(cave)
    Total += "}"

    #Total += HEATMAPHEAD
    ## heatmap for complaints
    #heat = """function getPoints() {
    #            return [ """
    #heatTmp = "\tnew google.maps.LatLng(%s, %s), \n"

    #for x in complaint:
    #    heat += heatTmp % (x[-4], x[-3])

    #heat += "\tnew google.maps.LatLng(%s, %s)" % (
    #    complaint[-1][-4], complaint[-1][-3])
    #heat += """];
    #    }"""

    #Total += heat


    Total += END

    return Total

if __name__ == '__main__':
    poth = []
    cave = []
    truck = []
    with open("poth.csv", 'rb') as tcsv:
        reader = csv.reader(tcsv, delimiter=',', quotechar='|')
        poth = list(reader)
    with open("cave.csv", 'rb') as scsv:
        reader = csv.reader(scsv, delimiter=',', quotechar='|')
        cave = list(reader)
    with open("truck_steet.csv", 'rb') as scsv:
        reader = csv.reader(scsv, delimiter=',', quotechar='|')
        truck = list(reader)
        print truck[1]

    prand = random.sample(poth, 100)
    crand = random.sample(cave, 100)
    trand = random.sample(truck, 100)
    jscript = generator(prand, crand, trand)
    f = open("truck_street.html", 'w')
    f.write(jscript)
    f.close()
    # print jscript
