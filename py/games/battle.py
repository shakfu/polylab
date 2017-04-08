import gambit

solver = gambit.nash.ExternalEnumPureSolver()

g = gambit.Game.new_table([2,2])

g.title = 'Battle of the Sexes'


g.players[0].label = 'Amy'
g.players[1].label = 'Bob'

g[0,0][0] = 3
g[0,0][1] = 1
g[0,1][0] = 0
g[0,1][1] = 2

g[1,0][0] = 2
g[1,0][1] = 1
g[1,1][0] = 0
g[1,1][1] = 3

print solver.solve(g)








