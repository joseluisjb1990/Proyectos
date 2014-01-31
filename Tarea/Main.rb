#!/usr/bin/env ruby


#########################################
### Elaborado por:                    ###
###  		Joel Araujo     10-10797  ###
### 		Jose Jimenez    10-10839  ###
###         Jose Montenegro 10-106    ###
#########################################


#Clase Padre Maquina, todas las demas maquinas heredan de ella
class Maquina

	#Diccionario de Estados que una maquina puede tener
	@@NUM_ESTADO = { 1 => "inactiva", 2 => "procesando", 3 => "en espera", 4 =>"llena" }

	attr_accessor :cicloActual
	attr_accessor :pAnterior
	attr_accessor :estado
	attr_accessor :maquinaA

	def initialize (cantMax, cantPA, desecho, cicloMax)

		@cantMax 	 = cantMax
	    @cantPA		 = cantPA
		@desecho 	 = desecho
		@cicloMax	 = cicloMax
		@cicloActual = 0
		@estado		 = 1
		@maquinaA	 = nil
		@cantProduc	 = 0
	end

	def procesar
		case @estado
			when 1
				if @maquinaA != nil					
					if @maquinaA.getProvisiones(@cantPA) 
						@estado = 4
					end
				else
					@estado = 4
				end
			when 2
				@cicloActual += 1
				if @cicloActual == @cicloMax
					@estado 	= 3
					@cantProduc = @cantMax * (1 - @desecho)
					@cicloActual = 0
				end

			when 3
				if @cantProduc == 0
					@estado = 1
				end
			when 4
				@estado = 2
		end
	end	

	def getProvisiones(cant)
		case @estado
			when 3 
				if cant <= @cantProduc
					@cantProduc -= cant
					true
				else
					self.estado = 1
					false
				end
			else false
		end	
	end				
	def to_s
		estado = @@NUM_ESTADO[self.estado]
		"Estado = " + estado + " Producto Actual = #@cantProduc"
	end
end


#Modulo que se encarga de la Cevada
module Cevada

	@@cevadaTotal = 0

	def maximoCevada(cevadaTotal)
		@@cevadaTotal = cevadaTotal
	end

	def procesaCevada
		@@cevadaTotal = @@cevadaTotal - @cevada
	end   
end


#Modulo que se encarga de la Mezcla de Arroz/Maiz
module Mezcla

	@@mezclaTotal = 0

	def maximoMezcla(mezclaTotal)
		@@mezclaTotal = mezclaTotal
	end

	def procesaMezcla
		@@mezclaTotal = @@mezclaTotal - @mezcla
	end   
end


#Modulo que se encarga de la Lupulo
module Lupulo

	@@lupuloTotal = 0

	def maximoLupulo(lupuloTotal)
		@@lupuloTotal = lupuloTotal
	end

	def procesaLupulo
		@@lupuloTotal = @@lupuloTotal - @lupulo
	end   
end


#Modulo que se encarga de la Levadura
module Levadura

	@@levaduraTotal = 0

	def maximoLevadura(levaduraTotal)
		@@levaduraTotal = levaduraTotal
	end

	def procesaCevada
		@@levaduraTotal = @@levaduraTotal - @levadura
	end   
end


#Modulo para inicializar los insumos de entrada
module InicializarInsumos
	include Cevada, Mezcla, Levadura, Lupulo

	def maximoInsumos(cevadaMax,mezclaMax,levaduraMax,lupuloMax)
		maximoCevada(cevadaMax)
		maximoMezcla(mezclaMax)
		maximoLevadura(levaduraMax)
		maximoLupulo(lupuloMax)
	end
end


#Clase de la Maquina "Silos de Cebada"
class Silos < Maquina 
	include Cevada


	def initialize

		super(cantMax = 400, cantPA = 0, desecho = 0, cicloMax = 0)
		@cevada = 400

	end 
end


#Clase de la Maquina "Molino"
class Molino < Maquina

	def initialize

		super(cantMax = 100, cantPA = 100, desecho = 0.02, cicloMax = 1)
		
	end 
end


#Clase de la Maquina "Paila de Mezcla"
class PailaMezcla < Maquina
	include Mezcla

	def initialize

		super(cantMax = 150, cantPA = 150*0.6, desecho = 0, cicloMax = 2)
		@mezcla	    = 0

		
	end 
end


#Clase de la Maquina "Cuba de Filtracion"
class Cuba < Maquina

	def initialize

		super(cantMax = 135, porcPA = 135, desecho = 0.35, cicloMax = 2)
			
	end 
end


#Clase de la Maquina "Paila de Coccion"
class PailaCoccion < Maquina
	include Lupulo

	def initialize

		super(cantMax = 70, porcPA = 70*0.975, desecho = 0.1, cicloMax = 3)
		@lupulo 	= 70*0.025
				
	end 
end


#Clase de la Maquina "Tanque pre-Clarificador"
class Tanque < Maquina

	def initialize

		super(cantMax = 35, porcPA = 35, desecho = 0.01, cicloMax = 0)
				
	end 
end


#Clase de la Maquina "Enfiador"
class Enfriador < Maquina
	
	def initialize

		super(cantMax = 60, porcPA = 60, desecho = 0, cicloMax = 2)
						
	end 
end


#Clase de la Maquina "TCC"
class TCC < Maquina
	include Levadura

	def initialize

		super(cantMax = 200, porcPA = 200*0.99, desecho = 0.1, cicloMax = 10)
		@levadura 	= 200*0.01

	end 
end


#Clase de la Maquina "Filtro de Cerveza"
class Filtro < Maquina
	
	def initialize

		super(cantMax = 100, porcPA = 100, desecho = 0, cicloMax = 1)
				
	end 
end


#Clase de la Maquina "Tanques para Cerveza Filtrada"
class CervezaFiltrada < Maquina
	
	def initialize

		super(cantMax = 100, porcPA = 100, desecho = 0, cicloMax = 1)
					
	end 
end


#Clase de la Maquina "Llenadora y Tapadora"
class Empacador < Maquina
	
	def initialize

		super(cantMax = 50, porcPA = 50, desecho = 0, cicloMax = 2)
					
	end 
end





#####################################################
###               Main del programa               ### 
#####################################################


include InicializarInsumos

unless ARGV.length == 5
    puts "\n Cantidad de argumentos invalido, Forma Correcta: "
    puts "\n ./main.rb <numero de ciclos> <cantidad cevada> <cantidad mezcla arroz/maiz> 
         <cantidad de levadura> <cantidad de lupulo> \n\n"
	exit
end

#Inicializamos las cantidades maximas de los insumos
nCiclos     = ARGV[0].to_i
cevadaMax   = ARGV[1].to_i
mezclaMax   = ARGV[2].to_i
levaduraMax = ARGV[3].to_i
lupuloMax   = ARGV[4].to_i

maximoInsumos(cevadaMax, mezclaMax, levaduraMax, lupuloMax)

#Instanciamos las clases
silos     = Silos.new

molino    = Molino.new
molino.maquinaA = silos

pailaM    = PailaMezcla.new
pailaM.maquinaA = molino

cuba 	  = Cuba.new
cuba.maquinaA = pailaM

pailaC    = PailaCoccion.new
pailaC.maquinaA = cuba

tanque    = Tanque.new
tanque.maquinaA = pailaC

enfriador = Enfriador.new
enfriador.maquinaA = tanque

tcc 	  	= TCC.new
tcc.maquinaA = enfriador

filtro    = Filtro.new
filtro.maquinaA = tcc

cervezaF  = CervezaFiltrada.new
cervezaF.maquinaA = filtro

empacador = Empacador.new
empacador.maquinaA = cervezaF

maquinas = [silos, molino, pailaM, cuba, pailaC, tanque, enfriador, tcc, filtro, cervezaF, empacador]




i=1
#Ciclos que se recorren
while i <= nCiclos
	
	print "CICLO ", i,"\n"
 
	for maq in maquinas
		maq.procesar
		puts maq
	end
	i += 1

end