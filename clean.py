import os
import csv
import sgf
import math
import numpy as np
import sys

def coord_quadrant(a):
  x = ord(a[0])-96
  y = ord(a[1])-96
  # upper left: 1
  if x <= 10 and y <= 10:
    return 1
  # upper right: 2
  elif x > 10 and y <= 10:
    return 2
  # lower left: 3
  elif x <= 10 and y > 10:
    return 3
  # lower right: 4
  elif x > 10 and y > 10:
    return 4

def coord_distance(a, b):
  return math.sqrt((ord(a[0]) - ord(b[0]))**2 +(ord(a[1]) - ord(b[1]))**2)

games = [['black_rating', 'white_rating', 'komi', 'winner', 'black_moves', 'white_moves', 'black_avg_subsequent_distance', 'white_avg_subsequent_distance', 'black_t1_quad', 'black_t2_quad', 'black_t3_quad', 'white_t1_quad', 'white_t2_quad', 'white_t3_quad', 'black_avg_subsequent_dist_from_white', 'white_avg_subsequent_dist_from_black', 'black_unique_quads_110', 'black_unique_quads_1120', 'black_unique_quads_2130', 'black_unique_quads_3140', 'black_unique_quads_4150', 'black_unique_quads_5160', 'black_unique_quads_6170', 'black_unique_quads_7180', 'black_unique_quads_8190', 'black_unique_quads_91100', 'black_unique_quads_101110', 'black_unique_quads_111210', 'black_unique_quads_121130', 'black_unique_quads_131140', 'black_unique_quads_141150', 'black_unique_quads_151160', 'black_unique_quads_161170', 'black_unique_quads_171180', 'black_unique_quads_181190', 'black_unique_quads_191200', 'white_unique_quads_110', 'white_unique_quads_1120', 'white_unique_quads_2130', 'white_unique_quads_3140', 'white_unique_quads_4150', 'white_unique_quads_5160', 'white_unique_quads_6170', 'white_unique_quads_7180', 'white_unique_quads_8190', 'white_unique_quads_91100', 'white_unique_quads_101110', 'white_unique_quads_111210', 'white_unique_quads_121130', 'white_unique_quads_131140', 'white_unique_quads_141150', 'white_unique_quads_151160', 'white_unique_quads_161170', 'white_unique_quads_171180', 'white_unique_quads_181190', 'white_unique_quads_191200']]

if len(sys.argv) != 2:
  print('pleaes provide name of sub folder')
  exit()

folder = sys.argv[1]

subfolders = os.listdir('games/' + folder)

for subfolder in subfolders:
  if subfolder == '.DS_Store': continue
  match_files = os.listdir('games/' + folder + '/' + subfolder)

  for match_file in match_files:
    if not match_file.endswith('.sgf'): continue
    with open('games/' + folder + '/' + subfolder + '/' + match_file) as f:
      print(match_file)

      try:
        collection = sgf.parse(f.read())
        root = collection.children[0].root.properties
        
        black_moves = 0
        white_moves = 0

        last_black_loc = ''
        last_white_loc = ''

        black_dist_total = 0
        white_dist_total = 0

        black_t1_quad = 0
        black_t2_quad = 0
        black_t3_quad = 0

        white_t1_quad = 0
        white_t2_quad = 0
        white_t3_quad = 0

        black_dist_from_white_total = 0
        white_dist_from_black_total = 0

        black_quads = []
        white_quads = []

        black_unique_quads = []
        white_unique_quads = []

        if collection.children[0].rest is None: continue

        rest = enumerate(collection.children[0].rest)
        for index, node in rest:
          props = node.properties
          side = list(props.keys())[0]
          location = props[side][0]

          if side == 'B':
            black_moves += 1
            if len(last_black_loc) > 0:
              black_dist_total += coord_distance(last_black_loc, location)
            
            if black_moves == 1:
              black_t1_quad = coord_quadrant(location)
            elif black_moves == 2:
              black_t2_quad = coord_quadrant(location)
            elif black_moves == 3:
              black_t3_quad = coord_quadrant(location)

            if len(last_white_loc) > 0:
              black_dist_from_white_total += coord_distance(last_white_loc, location)

            if black_moves > 10 and black_moves % 10 == 1:
              black_unique_quads.append(len(np.unique(black_quads)))
              black_quads = []
            black_quads.append(coord_quadrant(location))

            last_black_loc = location
          elif side == 'W':
            white_moves += 1
            if len(last_white_loc) > 0:
              white_dist_total += coord_distance(last_white_loc, location)

            if white_moves == 1:
              white_t1_quad = coord_quadrant(location)
            elif white_moves == 2:
              white_t2_quad = coord_quadrant(location)
            elif white_moves == 3:
              white_t3_quad = coord_quadrant(location)

            if len(last_black_loc) > 0:
              white_dist_from_black_total += coord_distance(last_black_loc, location)

            if white_moves > 10 and white_moves % 10 == 1:
              white_unique_quads.append(len(np.unique(white_quads)))
              white_quads = []
            white_quads.append(coord_quadrant(location))

            last_white_loc = location

        if len(black_unique_quads) > 0:
          black_unique_quads.append(len(np.unique(black_quads)))
        if len(black_unique_quads) > 0:
          white_unique_quads.append(len(np.unique(white_quads)))

        black_unique_quads = np.array(black_unique_quads)
        black_unique_quads.resize(1,20)

        white_unique_quads = np.array(white_unique_quads)
        white_unique_quads.resize(1,20)

        if black_moves == 0 or white_moves == 0: continue

        game = [
        root['BR'][0] if root.get('BR') is not None else None,
        root['WR'][0] if root.get('WR') is not None else None,
        root['KM'][0] if root.get('KM') is not None else None,
        root['RE'][0][0] if root.get('RE') is not None else None,
        black_moves,
        white_moves,
        black_dist_total/black_moves,
        white_dist_total/white_moves,
        black_t1_quad,
        black_t2_quad,
        black_t3_quad,
        white_t1_quad,
        white_t2_quad,
        white_t3_quad,
        black_dist_from_white_total/(black_moves-1) if black_moves > 1 else None, #black goes first
        white_dist_from_black_total/white_moves if white_moves > 0 else None,
        ]

        game = game + list(black_unique_quads[0])
        game = game + list(white_unique_quads[0])

        #print(game)

        games.append(game)
      except:
        print('bad file, skipped')

with open('go_data_' + folder + '.csv', 'w+') as f:
  wr = csv.writer(f, delimiter=',')
  wr.writerows(games)
  print('finished parsing data')