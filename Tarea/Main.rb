#!/usr/bin/env ruby

class Maquina
	@@NUM_ESTADO = { 1 => "inactiva", 2 => "procesando", 3 => "en espera", 4 =>"llena" }

	attr_accessor :cicloActual
	attr_accessor :pAnterior
	attr_accessor :estado

	def initialize (cantMax, cantPA, desecho, cicloMax)

		@cantMax 	 = cantMax
	    @cantPA		 = cantPA
		@desecho 	 = desecho
		@cicloMax	 = cicloMax
		@cicloActual = 0
		@estado		 = 1
		
	end

	def to_s
		estado = @@NUM_ESTADO[@estado]
		"Cantidad maxima = #@cantMax\nDesecho = #@desecho \nCiclos = #@ciclos \nEstado = " + estado + "\nProducto Anterior = #@pAnterior"
	end
end


module Cevada

	@@cevadaTotal = 0

	def maximo(cevadaTotal)
		@@cevadaTotal = cevadaTotal
	end

	def procesaCevada
		@@cevadaTotal = @@cevadaTotal - @cevada
	end   
end


module Mezcla

	@@mezclaTotal = 0

	def maximo(mezclaTotal)
		@@mezclaTotal = mezclaTotal
	end

	def procesaMezcla
		@@mezclaTotal = @@mezclaTotal - @mezcla
	end   
end


module Lupulo

	@@lupuloTotal = 0

	def maximo(lupuloTotal)
		@@lupuloTotal = lupuloTotal
	end

	def procesaLupulo
		@@lupuloTotal = @@lupuloTotal - @lupulo
	end   
end


module Levadura

	@@levaduraTotal = 0

	def maximo(levaduraTotal)
		@@levaduraTotal = levaduraTotal
	end

	def procesaCevada
		@@levaduraTotal = @@levaduraTotal - @levadura
	end   
end


class Silos < Maquina 
	include Cevada


	def initialize

		super(cantMax = 400, cantPA = 0, desecho = 0, cicloMax = 0)
		@cevada = 400

	end 
end



class Molino < Maquina

	def initialize

		super(cantMax = 100, cantPA = 100, desecho = 0.02, cicloMax = 1)
		
	end 
end


class PailaMezcla < Maquina
	include Mezcla

	def initialize

		super(cantMax = 150, cantPA = 150*0.6, desecho = 0, cicloMax = 2)
		@insumo	    = 0

		
	end 
end


class Cuba < Maquina

	def initialize

		super(cantMax = 135, porcPA = 135, desecho = 0.35, cicloMax = 2)
			
	end 
end



class PailaCoccion < Maquina
	include Lupulo

	def initialize

		super(cantMax = 70, porcPA = 70*0.975, desecho = 0.1, cicloMax = 3)
		@lupulo 	= 70*0.025
				
	end 
end


class Tanque < Maquina

	def initialize

		super(cantMax = 35, porcPA = 35, desecho = 0.01, cicloMax = 1)
				
	end 
end

class Enfriador < Maquina
	
	def initialize

		super(cantMax = 60, porcPA = 60, desecho = 0, cicloMax = 2)
						
	end 
end


class TCC < Maquina
	include Levadura

	def initialize

		super(cantMax = 200, porcPA = 200*0.99, desecho = 0.1, cicloMax = 10)
		@levadura 	= 200*0.01

	end 
end


class Filtro < Maquina
	
	def initialize

		super(cantMax = 100, porcPA = 100, desecho = 0, cicloMax = 1)
				
	end 
end


class CervezaFiltrada < Maquina
	
	def initialize

		super(cantMax = 100, porcPA = 100, desecho = 0, cicloMax = 0)
					
	end 
end


class Empacador < Maquina
	
	def initialize

		super(cantMax = 50, porcPA = 50, desecho = 0, cicloMax = 2)
					
	end 
end






#MAiNNNNNNNNNNNNNNN

i = 0

silos     = Silos.new
molino    = Molino.new
pailaM    = PailaMezcla.new
cuba 	  = Cuba.new
pailaC    = PailaCoccion.new
tanque    = Tanque.new
enfriador = Enfriador.new
tcc 	  = TCC.new
filtro    = Filtro.new
cervezaF  = CervezaFiltrada.new
empacador = Empacador.new

maquinas = [silos, molino, pailaM, cuba, pailaC, tanque, enfriador, tcc, filtro, cervezaF, empacador]


nCiclos=5

while i < nCiclos
	
	

	i += 1

end