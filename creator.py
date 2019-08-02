wall = [(0,0)]
pill = [(1,1)]
empty= [(0,1)]

dg1 = 14 * wall
dg2 = wall + 12*pill + wall
dg3 = wall + pill + 4*wall + pill + 5*wall + pill + wall
dg4 = wall + pill + 4*wall + pill + 5*wall + pill + wall
dg5 = wall + pill + 4*wall + pill + 5*wall + pill + wall
dg6 = wall + 12*pill + wall
dg7 = wall + pill + 4*wall + pill + 2*wall + pill + 4*wall
dg8 = wall + pill + 4*wall + pill + 2*wall + pill + 4*wall
dg9 = wall + 6*pill + 2*wall + 4*pill + wall
dg10 = 6*wall + pill + 5*wall + empty + wall
dg11 = 6*wall + pill + 5*wall + empty + wall
dg12 = 6*wall + pill + 2*wall + 5*empty
dg13 = 6*wall + pill + 2*wall + 5*empty
dg14 = 6*wall + pill + 5*wall + pill + wall
dg15 = 14*pill
dg16 = 14*wall


def mkrow(row):
  return row + list(reversed(row))

print(list(map(mkrow, [dg1, dg2, dg3, dg4, dg5, dg6, dg7, dg8, dg9, dg10, dg11, dg12, dg13, dg14, dg15, dg16])))