type ('q, 's) automate = ('a -> bool)*('a -> bool)*(a'*b'*'a list);;

type ('q, 's1, 's2) automatequiecrit = ('q*('s2 list), 's1 ) automate;;

type ('q, 'sm, 's1, 's2) automatequiecritavecmemoire = ('q*'sm,'s1,'s2) automatequiecrit

type monautomatequiecrit  = (int, char, char) automatequiecrit ;;
