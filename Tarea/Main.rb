#!/usr/bin/env ruby


#########################################
### Elaborado por:                    ###
###  		Joel Araujo     10-10797  ###
### 		Jose Jimenez    10-10839  ###
###         Jose Montenegro 10-10469  ###
#########################################


#Clase Padre Maquina, todas las demas maquinas heredan de ella
class Maquina

	#Diccionario de Estados que una maquina puede tener
	@@NUM_ESTADO = { 1 => "inactiva", 2 => "procesando", 3 => "en espera", 4 =>"llena" }

	attr_accessor :cicloActual
	attr_accessor :estado
	attr_accessor :maquinaA

	def initialize (nombreMaquina, cantMax, cantPA, desecho, cicloMax)
		@nombreMaquina = nombreMaquina
		@cantMax 	   = cantMax
		@cantPA		   = cantPA
		@desecho 	   = desecho
		@cicloMax	   = cicloMax
		@cicloActual   = 0
		@estado		   = 1
		@maquinaA	   = nil
		@cantProduc	   = 0
		@prodAlmacen   = 0
	end

	#def to_s
	#	puts "Maquina = <#{@nombreMaquina}>"
	#	puts "Estado  = <#{@estado}>"
	#end 

	#Se encarga de realizar todo los movimientos posibles de una maquina
	def procesar
			case @estado

			#Estado = Inactiva, la maquina se llena	
			when 1
				if @maquinaA != nil
					if @prodAlmacen < @cantPA			
						@prodAlmacen +=  @maquinaA.getProvisiones(@cantPA - @prodAlmacen) 
						if @prodAlmacen == @cantPA
							@estado = 4
						else
							@estado = 1	
						end
					else
						@estado = 4
					end
				else
					@estado = 4
				end

			#Estado = Procesando, la maquina esta procesando su insumo
			when 2
				@cicloActual += 1
				if @cicloActual == @cicloMax
					@estado 	= 3
					@cantProduc = @cantProduc + (@cantMax * (1 - @desecho))
					@prodAlmacen = 0
					@cicloActual = 0
				end

			#Estado = En espera, la maquina esta esperando a que la 
			#proxima maquina este inactiva para podor darle insumos
			when 3
				if @cantProduc == 0
					@estado = 1
				end

			#Estado = Lleno, la maquina esta lista para procesar su
			when 4
				if @cicloMax == 0
					@cantProduc = @cantProduc + @cantMax * (1 - @desecho)
					@prodAlmacen = 0
					@estado = 3
				else
					@estado = 2
				end
		end
	end	

	#Revisa si la maquina anterior tiene insumos para suministrarle
	def getProvisiones(cant)
		case @estado
			when 3 
				if cant <= @cantProduc
					@cantProduc -= cant
					return cant
				else
					out = @cantProduc
					@cantProduc = 0
					@estado = 1
					return out
				end
			else 0
		end	
	end				
	def to_s
		estado = @@NUM_ESTADO[self.estado]
		"Estado = " + estado + " #@cantProduc #@prodAlmacen #@cantPA"
	end
end


#Modulo que se encarga de la Cebada
module Cebada

	class CebadaInsuficiente < RuntimeError; end

	@@cebadaTotal = 0

	def maximoCebada(cebadaTotal)
		@@cebadaTotal = cebadaTotal
	end

	def procesaInsumo
		if @cebada > @@cebadaTotal
			raise CebadaInsuficiente
		end
		@@cebadaTotal = @@cebadaTotal - @cebada
		puts "#@@cebadaTotal"
	end   

	def printCebada
		puts "Cebada Sobrante = #{@@cebadaTotal}"
	end
end


#Modulo que se encarga de la Mezcla de Arroz/Maiz
module Mezcla

	class MezclaInsuficiente < RuntimeError; end
	@@mezclaTotal = 0

	def maximoMezcla(mezclaTotal)
		@@mezclaTotal = mezclaTotal
	end

	def procesaInsumo
		if @mezcla > @@mezclaTotal
			raise MezclaInsuficiente
		end
		@@mezclaTotal = @@mezclaTotal - @mezcla
		puts "#@@mezclaTotal"
	end   

	def printMezcla
		puts "Mezcla Sobrante = #{@@mezclaTotal}"
	end
end


#Modulo que se encarga de la Lupulo
module Lupulo

	class LupuloInsuficiente < RuntimeError; end

	@@lupuloTotal = 0

	def maximoLupulo(lupuloTotal)
		@@lupuloTotal = lupuloTotal
	end

	def procesaInsumo
		if @lupulo > @@lupuloTotal
			raise LupuloInsuficiente
		end
		@@lupuloTotal = @@lupuloTotal - @lupulo
		puts "#@@lupuloTotal"
	end  

	def printLupulo
		puts "Lupulo Sobrante = #{@@lupuloTotal}"
	end 
end


#Modulo que se encarga de la Levadura
module Levadura

	class LevaduraInsuficiente < RuntimeError; end

	@@levaduraTotal = 0

	def maximoLevadura(levaduraTotal)
		@@levaduraTotal = levaduraTotal
	end

	def procesaInsumo
		if @levadura > @@levaduraTotal
			raise LevaduraInsuficiente
		end
		@@levaduraTotal = @@levaduraTotal - @levadura
		puts "#@@levaduraTotal"
	end

	def printLevadura
		puts "Levadura Sobrante = #{@@levaduraTotal}"
	end
end

#Modulo para inicializar los insumos de entrada
module InicializarInsumos
	include Cebada, Mezcla, Levadura, Lupulo

	def maximoInsumos(cebadaMax,mezclaMax,levaduraMax,lupuloMax)
		maximoCebada(cebadaMax)
		maximoMezcla(mezclaMax)
		maximoLevadura(levaduraMax)
		maximoLupulo(lupuloMax)
	end

	def printSobrantes
		printCebada
		printLupulo
		printLevadura
		printMezcla
	end
end


#Clase de la Maquina "Silos de Cebada"
class Silos < Maquina 
	include Cebada


	def initialize
		super(nombreMaquina = "Silos de Cebada", 
			  cantMax = 400, cantPA = 0, desecho = 0, cicloMax = 0)
		@cebada = 400
	end


	def procesar
		estadoAn = @estado
		super
		if (estadoAn == 1 && @estado == 4)
			procesaInsumo
		end
	end 
end


#Clase de la Maquina "Molino"
class Molino < Maquina


	def initialize
		super(nombreMaquina = "Molino", 
		      cantMax = 100, cantPA = 100, desecho = 0.02, cicloMax = 1)	
	end 
end


#Clase de la Maquina "Paila de Mezcla"
class PailaMezcla < Maquina
	include Mezcla


	def initialize
		super(nombreMaquina = "Paila de Mezcla", 
			  cantMax = 150, cantPA = 150*0.6, desecho = 0, cicloMax = 2)
		@mezcla	    = 150*0.4	
	end


	def procesar
		estadoAn = @estado
		super
		if (estadoAn == 1 && @estado == 4)
			procesaInsumo
		end
	end
end


#Clase de la Maquina "Cuba de Filtracion"
class Cuba < Maquina


	def initialize
		super(nombreMaquina = "Cuba de Filtracion", 
			  cantMax = 135, porcPA = 135, desecho = 0.35, cicloMax = 2)		
	end 
end


#Clase de la Maquina "Paila de Coccion"
class PailaCoccion < Maquina
	include Lupulo


	def initialize
		super(nombreMaquina = "Paila de Coccion", 
			  cantMax = 70, porcPA = 70*0.975, desecho = 0.1, cicloMax = 3)
		@lupulo 	= 70*0.025
				
	end


	def procesar
		estadoAn = @estado
		super
		if (estadoAn == 1 && @estado == 4)
			procesaInsumo
		end
	end
end


#Clase de la Maquina "Tanque pre-Clarificador"
class Tanque < Maquina


	def initialize
		super(nombreMaquina = "Tanque pre-Clarificador", 
			  cantMax = 35, porcPA = 35, desecho = 0.01, cicloMax = 0)			
	end 
end


#Clase de la Maquina "Enfiador"
class Enfriador < Maquina
	

	def initialize
		super(nombreMaquina = "Enfriador", 
			  cantMax = 60, porcPA = 60, desecho = 0, cicloMax = 2)					
	end 

end


#Clase de la Maquina "TCC"
class TCC < Maquina
	include Levadura


	def initialize
		super(nombreMaquina = "TCC", 
			  cantMax = 200, porcPA = 200*0.99, desecho = 0.1, cicloMax = 10)
		@levadura 	= 200*0.01

	end


	def procesar
		estadoAn = @estado
		super
		if (estadoAn == 1 && @estado == 4)
			procesaInsumo
		end
	end 

end


#Clase de la Maquina "Filtro de Cerveza"
class Filtro < Maquina
	

	def initialize
		super(nombreMaquina = "Filtro de Cerveza", 
			  cantMax = 100, porcPA = 100, desecho = 0, cicloMax = 1)			
	end 

end


#Clase de la Maquina "Tanques para Cerveza Filtrada"
class CervezaFiltrada < Maquina
	

	def initialize
		super(nombreMaquina = "Tanques para Cerveza Filtrada", 
			  cantMax = 100, porcPA = 100, desecho = 0, cicloMax = 1)				
	end 

end


#Clase de la Maquina "Llenadora y Tapadora"
class Empacador < Maquina
	
	attr_accessor :hayProducto
	def initialize
		super(nombreMaquina = "Llenadora y Tapadora", 
			  cantMax = 50, porcPA = 50, desecho = 0, cicloMax = 2)	
		@hayProducto = false
	end


	def procesar
		estadoAn = @estado
		super
		if (estadoAn == 2 && @estado == 3)
			puts @cantProduc
			@hayProducto = true
		end
	end


	def obtenerTotal
		prodTotal = @cantProduc
		@cantProduc = 0
		@hayProducto = false
		return prodTotal
	end
end

#####################################################
###               Main del programa               ### 
#####################################################


include InicializarInsumos

unless ARGV.length == 5
    puts "\n Cantidad de argumentos invalido, Forma Correcta: "
    puts "\n ./main.rb <numero de ciclos> <cantidad cebada> <cantidad mezcla arroz/maiz> 
         <cantidad de levadura> <cantidad de lupulo> \n\n"
	exit
end

#Inicializamos las cantidades maximas de los insumos
nCiclos     = ARGV[0].to_i
cebadaMax   = ARGV[1].to_i
mezclaMax   = ARGV[2].to_i
levaduraMax = ARGV[3].to_i
lupuloMax   = ARGV[4].to_i

maximoInsumos(cebadaMax, mezclaMax, levaduraMax, lupuloMax)

#Instanciamos las clases
silos = Silos.new

molino = Molino.new
molino.maquinaA = silos

pailaM = PailaMezcla.new
pailaM.maquinaA = molino

cuba = Cuba.new
cuba.maquinaA = pailaM

pailaC = PailaCoccion.new
pailaC.maquinaA = cuba

tanque = Tanque.new
tanque.maquinaA = pailaC

enfriador = Enfriador.new
enfriador.maquinaA = tanque

tcc = TCC.new
tcc.maquinaA = enfriador

filtro = Filtro.new
filtro.maquinaA = tcc

cervezaF  = CervezaFiltrada.new
cervezaF.maquinaA = filtro

empacador = Empacador.new
empacador.maquinaA = cervezaF

maquinas = [silos, molino, pailaM, cuba, pailaC, tanque, enfriador, tcc, filtro, cervezaF, empacador]


i=1
prodTotal = 0
#Ciclos que se recorren

catch(:escape) do
	while i <= nCiclos
		
		puts "\nInicio Ciclo <#{i}> \n\n"
		for maq in maquinas
			begin
				maq.procesar
			rescue CebadaInsuficiente => error
				puts "\nSe termino la cebada mientras corrian los ciclos"
				throw(:escape)
			rescue LupuloInsuficiente => error
				puts "\nSe termino el lupulo corrian los ciclos"
				throw(:escape)
			rescue MezclaInsuficiente => error
				puts "\nSe termino la mezcla mientras corrian los ciclos"
				throw(:escape)
			rescue LevaduraInsuficiente => error
				puts "\nSe termino la levadura mientras corrian los ciclos"
				throw(:escape)
			else
				puts maq
			end

		end
		if (empacador.hayProducto)
			prodTotal += empacador.obtenerTotal
		end

		puts "Producto total = ", prodTotal
		puts "\nFin Ciclo <#{i}>"
		i += 1
	end
end

#Print Final
puts "Ciclos= #{i}"
puts "Cerveza Total = "
puts printSobrantes