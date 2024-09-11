##PASO 01. INSTALAR PAQUETES Y LIBRERÍAS PARA IMPORTACIÓN Y LIMPIEZA DE DATOS

## Instalar y cargar paquetes para el análisis.

install.packages("tidyverse")

library(tidyverse)

installed.packages()

## Activar readxl para leer los archivos de excel
library(readxl)

## Cargar las bases de datos usando readxl
## Agregar a una variable diferente el nombre de cada pestaña

coffee_orders <- read_excel("C:/Users/Usuario/Desktop/Archivos/coffeeOrdersData_2024.xlsx", sheet = 1)

View(coffee_orders)

coffee_customers <- read_excel("C:/Users/Usuario/Desktop/Archivos/coffeeOrdersData_2024.xlsx", sheet = 2)

View(coffee_customers)

coffee_products <- read_excel("C:/Users/Usuario/Desktop/Archivos/coffeeOrdersData_2024.xlsx", sheet = 3)

View(coffee_products)



##Instalar las librerías para limpiar los datasets

library(readr)
library(tidyverse)
library(skimr)
library(janitor)


##Asegurar la integridad inicial de los datos mediante la limpieza de nombres

coffee_products <- clean_names(coffee_products)
coffee_customers <- clean_names(coffee_customers)
coffee_orders <- clean_names(coffee_orders)


##Revisar la limpieza de los nombres de las columnas

View(coffee_products)
View(coffee_customers)
View(coffee_orders)

##Ahora los nombres de las columnas pueden usarse dentro de funciones

##Verificar la existencia de datos duplicados y nulos

sum(is.na(coffee_products)) ##0 valores nulos
sum(duplicated(coffee_products$product_id)) ##0 valores duplicados

sum(is.na(coffee_orders)) ##2205 valores nulos

##Se cargaron dos columnas enteras con valores nulos, eliminarlas

coffee_orders <- coffee_orders %>% 
  select(-x16, -x17)

##Existen valores nulos en el campo email, ya que este no es un dato de importancia
## Para el análisis, simplemente se ignorará la columna

sum(duplicated(coffee_orders$order_id)) ##43 valores duplicados

##Con esto verifico que la existencia de valores duplicados es debido a que
##el id de orden aparece por cada producto diferente aunque esté dentro de la misma compra

duplicated_coffee_orders <- coffee_orders %>%
  group_by(order_id) %>% 
  filter(n() > 1)

View(duplicated_coffee_orders)

sum(duplicated(coffee_customers$customer_id))## 0 valores duplicados
sum(is.na(coffee_customers)) ##334 valores nulos, los campos con valores vacíos 
                             ##(e-mail y phone_number) no son relevantes para el análisis

##PASO 02. OBTENER INFORMACIÓN DE VALOR CON LOS DATOS

## Realizar algunas consultas exploratorias para conocer los datos

coffee_customers %>%  
  group_by(country) %>% 
  count(country)

## Los pedidos se dividen en tres países, 
## existen 150 ordenes provenientes de Irlanda, 68 de Reino Unido y 782 de Estados Unidos

##Cuantos clientes tienen loyalty_card

loyaltycard_customers <- coffee_customers %>%
  filter(loyalty_card == "Yes") %>%
  count()


View(loyaltycard_customers) ##Son 487 clientes con loyalty card

new_customers <- coffee_customers %>% 
  filter(loyalty_card == "No") %>% 
  count()

View(new_customers) ##Son 513 clientes nuevos o que no cuentan con loyalty card 

#Conteo de loyaltycard para crear gráfico

loyalty_card <- coffee_customers %>% 
  group_by(loyalty_card) %>% 
  count()
View(loyalty_card)

##Revisar cual es el producto más ordenado, el café favorito
#Asegurarse que el ID del producto está asociado a un solo producto, y que es igual para ambas tablas

productos_diferentes <- coffee_products %>% 
  group_by(product_id) %>% 
  count()

View(productos_diferentes)

favorite_coffee <- coffee_orders %>%
  group_by(product_id, coffee_type_name, roast_type_name, size) %>% 
  summarise(total_orders = sum(quantity)) %>% 
  arrange(desc(total_orders))
  
View(favorite_coffee)  

top_5_coffee <- favorite_coffee %>%
  arrange(desc(total_orders)) %>% 
    head(5)

View(top_5_coffee)


##Existen 48 productos diferentes, el más ordenado corresponde a:
## ID: R-L-0.2, Tipo Robusta, tostado Light, tamaño 0.2.


##El comportamiento de la demanda de cada producto según su precio

demanda_precio <- merge(favorite_coffee, coffee_products, by = "product_id") %>% 
  select(product_id, coffee_type_name, size.x, total_orders, unit_price)
View(demanda_precio)

cor(demanda_precio$unit_price, demanda_precio$total_orders)

##El resultado de la correlación es -0.17 lo cual indica una correlación negativa bastante débil
##Es decir que esta tendencia no es significativa.  En otras palabras, 
##el precio puede no ser un factor determinante para la cantidad de pedidos, 
##ya que la relación no es lineal. Una relación lineal es un valor positivo, un valor con una
##correlación fuerte es igual a 1.


##Revisar cual país y ciudad ordena más café

city_orders <- merge(coffee_customers, coffee_orders, by = "customer_id") %>%
  group_by(city, country.x) %>%
  summarise(total_quantity = sum(quantity)) %>% 
  arrange(desc(total_quantity))

View(city_orders)

country_orders <- merge(coffee_customers, coffee_orders, by = "customer_id") %>%
  group_by(country.x) %>% 
  summarise(total_quantity = sum(quantity)) %>% 
  arrange(desc(total_quantity))

View(country_orders)

##El país con mayor consumo de café es Estados Unidos(2760), y la ciudad de Washgington(90)

##Agregar una columna calculada para asignar porcentajes


porcentaje_compras <- mutate (country_orders, porcentaje = round(total_quantity/sum(total_quantity)*100, 2))

View(porcentaje_compras)

##Vemos que los porcentajes corresponden a EU:el 77.72%, Irleand: 15.12%, UK: 7.15

##Revisar los porcentajes de ventas que provienen de clientes con y sin loyalty card
##Crear un tibble solo con las columnas que se requieren para el cálculo

as_tibble <- loyaltycard.tb <- merge(coffee_customers, coffee_orders, by = "customer_id") %>% 
  select(country.x, loyalty_card, quantity, sales)
  
View(loyaltycard.tb)

##Agrupar por lc y obtener los totales de cantidad y ventas

porcentajes_loyaltycard <- loyaltycard.tb %>%
  group_by(loyalty_card) %>% 
  summarise(
    total_quantity = sum(quantity),
    total_sales = sum(sales))

View(porcentajes_loyaltycard)

##El total de ventas.
##Obtener los totales en general de las cantidades y ventas

total_quantity_sum <- sum(porcentajes_loyaltycard$total_quantity)
print(total_quantity_sum)
total_sales_sum <- sum(porcentajes_loyaltycard$total_sales)
print(total_sales_sum)



##PASO 03. REALIZAR ANÁLISIS ESTADÍSTICOS

##Agregar las columnas calculadas para los porcentajes

porcentajes_loyaltycard <- porcentajes_loyaltycard %>% 
  mutate(
    quantity_percentage = round((total_quantity/total_quantity_sum)*100, 2),
    sales_percentage = round((total_sales/total_sales_sum)*100, 2))
View(porcentajes_loyaltycard)

##Los porcentajes para cada una varían por muy poco: 
## sales without lc: 53.65%, cantidad de paquetes without lc: 53.11%
## sales with lc: 46.34%, cantidad de paquetes with lc: 46.88%
                                  
##Ya que los ´porcentajes de totales difieren por muy poco
##Hacer una prueba de significancia estadística para definir si la diferencia es significativa
##Mediante esto se puede definir si las estrategias de marketing deben ser enfocadas principalmente
##a los clientes nuevos o a obtener recompras por clientes afiliados

##Crear dos variables para con y sin tarjeta de lealtad

clientes_con_tarjeta <- loyaltycard.tb %>% filter(loyalty_card == "Yes")
clientes_sin_tarjeta <- loyaltycard.tb %>% filter(loyalty_card == "No")

##Aplicar la prueba t de student para calcular la significancia estadística de las dos variables

significancia_estadística <- t.test(clientes_con_tarjeta$sales, clientes_sin_tarjeta$sales)
print(significancia_estadística)

##Según los resultados obtenidos no hay diferencia estadísticamente significativa entre ambos grupos
##La media para el grupo con lc: 43.66, sin cl: 46.48
##El valor p obtenido: 0.2935, muy lejos de 0.05 requerido para rechazar la hipótesis nula (hay diferencia significativa)
##Por lo que se acepta la hipótesis alterna: No ha diferencia estadísticamente significativa a un 95% de confianza


##El tamaño de café que más se pide y el que más se vende

product_orders <- coffee_orders %>%
  group_by(size) %>% 
  summarise(max_sales = sum(sales),
            max_quantity = sum(quantity)) %>% 
  arrange(desc(max_sales))
View(product_orders)
print(max(product_orders$max_quantity))
print(max(product_orders$max_sales))


##Se manejan 4 tamaños: 0.2, 0.5, 1, 2.5 
##El tamaño y tipo de café más pedido es s:0.2, con un tota de 943 unidades vendidas, y un total de ventas de 7029.99
##sin embargo el café que genera más dinero en ventas es el de tamaño 2.5 con un total de 841 unidades, y el total de ventas de 23785.56

                
##Obtener la ganancia neta generada por cada producto

sales_per_product <- coffee_orders %>% 
  group_by(product_id) %>% 
  summarise(total_quantity = sum(quantity)) %>% 
  arrange(desc(total_quantity))

View(sales_per_product)

product_profit <- merge(coffee_products, sales_per_product, by = "product_id") %>%
  mutate(total_profit = total_quantity * profit) %>%
  select(product_id, coffee_type, size, unit_price, profit, total_profit) %>% 
  arrange(desc(total_profit))

View(product_profit)

##Calcular el total total de las sumas del profit

total_profit_sum <- sum(product_profit$total_profit)
print(total_profit_sum)

##El total de ganancias netas es de 4520.217

##Calcular el porcentaje de ganancias correspondiente para los 5 productos más vendidos

top_5_products <- product_profit %>%
  arrange(desc(total_profit)) %>% 
  head(5)


top_5_percentage <- product_profit %>% 
  mutate(percentage_profit =
    (total_profit/total_profit_sum)*100) %>%
  arrange(desc(percentage_profit)) %>% 
  head(5)

View(top_5_percentage)
print(top_5_percentage)

##Porcentaje total de ventas entre los 5 productos que generan mayor cantidad de ingresos
sum_percentage_top_5 <- top_5_percentage %>% 
  summarise(representative_persentage = sum(percentage_profit))
print(sum_percentage_top_5)


##TOP 5 CLIENTES CON MAYOR CANTIDAD DE ORDENES DE COMPRA

top_5_customers <- coffee_orders %>% 
  group_by(customer_id) %>% 
  summarise(num_orders = n_distinct(order_id)) %>% 
  filter(num_orders > 1) %>% 
  arrange(desc(num_orders)) %>% 
  head(5)

View(top_5_customers)

top_5_customers_info <- left_join(coffee_customers, top_5_customers, by = "customer_id") %>% 
  select(customer_id, num_orders, loyalty_card, city) %>% 
  filter(!is.na(num_orders)) ## para filtrar solo los que aparecen en num_orders

View(top_5_customers_info)


##Obtener datos para conocer la probabilidad de recompra
##Calcular cuantas órdenes diferentes ha hecho cada cliente

customer_orders_count <- coffee_orders %>%
  group_by(customer_id) %>% 
  summarise(num_orders = n_distinct(order_id)) %>%
  arrange(desc(num_orders))

View(customer_orders_count)

customer_orders_total <- merge(customer_orders_count, coffee_customers, by = "customer_id") %>% 
  select(customer_id, num_orders, loyalty_card)
View(customer_orders_total)

##Combinar la tabla con coffee_orders para saber que producto adquirió y cuanto gastó

customers_total_orders_info <- merge(customer_orders_total, coffee_orders, by = "customer_id") %>% 
  select(customer_id, order_id, num_orders, loyalty_card, coffee_type_name, sales) %>% 
  group_by(order_id)

View(customers_total_orders_info)

##Calcular la probabilidad de recompra con una regresión logística, basado en tiene o no tiene loyalty_card
##Utilizando customer_orders_total

##Asignar un número binario a la variable loyaltycard, 1=Yes, 0=No
##Y de acuerdo a cuantas veces ha comprado el cliente

customer_orders_total <- customer_orders_total %>% 
  mutate(buyback = ifelse(num_orders > 1, 1, 0)) ## si num_orders es mayor a 1, se asigna 1, de lo contrario, 0
View(customer_orders_total)

##Una vez establecido el valor binominal de la variable, establecer el modelo de regresión logística

modelo_logit <- glm(buyback ~ loyalty_card, data = customer_orders_total, family = "binomial")

summary(modelo_logit)

predicted_probabilities <- predict(modelo_logit, type = "response")

head(predicted_probabilities)

##Según los resultados del modelo de predicción logística tenemos que:
##La razón de probabilidad cuando LC = No es de -3.64 = probabilidad muy baja de recompra
##La razón de probabilidad cuando LC = Yes es de 0.1431 = probabilidad baja, ligeramente mayor a la anterior
##El valor p para estos valores es igual a 0.74, lo que indica que la diferencia no es significativa
##Por lo que las probabilidades de recompra no dependen de contar o no con una LC
##Para que las probabilidades de recompra sean significativas el valor logarítmico debe ser mayor a 0
##log odds=0 indica un 50% de probabilidad
##log odds=1 indica un 73%, lo ideal es obtener un valor mayor a 1
##log odds=2= 88%, =3= 95%

##Para que el valor p sea significativo entre el poseer o no una LC, debe ser igual o menor a 0.05

##Realizar un gráfico de cajas y bigotes pra comparar la probabilidad de recompra
##Según tengan o no loyaltycard

ggplot(customer_orders_total, aes(x = loyalty_card, y = num_orders, fill = loyalty_card)) +
  geom_boxplot() +
  labs(title = "Distribución de total de compras por tipo de cliente",
       x = "Tarjeta de lealtad", y = "Total de compras") +
  theme_minimal()

##El gráfico no sirve como una buena representaciónm ya que las cantidades se muestran demasiado unificadas
##Dejar la explicación estadística únicamente como un informe


##PASO 04. OBTENER DATOS DE ACUERDO A FECHAS

##Asegurarse de que la columna tiene formato de fecha

coffee_orders$order_date <- as.Date(coffee_orders$order_date, format = "%Y-%m-%d")
str(coffee_orders$order_date)

View(coffee_orders)
str(coffee_orders)

##verificar que no hay valores nulos

sum(is.na(coffee_orders$order_date)) ## no hay valores nulos


##Agregar dos columnas nuevas que contengan por separado únicamente el mes, año

coffee_orders_dates <- coffee_orders %>% 
  mutate(year = format(order_date, "%Y"),
         month = format(order_date, "%m-%Y"))

View(coffee_orders_dates)

##Calcular el total de ventas por año

yearly_sales <- coffee_orders_dates %>% 
  group_by(year) %>% 
  summarise(sales_per_year = sum(sales, na.rm = TRUE))

print(yearly_sales)

monthly_sales <- coffee_orders_dates %>% 
  group_by(month, year) %>% 
  summarise(sales_per_month = sum(sales, na.rm = TRUE))

print(monthly_sales)


##PASO 05. CREAR GRÁFICAS

install.packages("scales")
library(scales)

##Ventas por año, GRÁFICO DE BARRAS

yearly_sales$year <- as.factor(yearly_sales$year)
ggplot(yearly_sales, aes(x = sales_per_year, y = year))+
  geom_bar(stat = "identity", fill = "skyblue")+
  geom_text(aes(label = sales_per_year), hjust = -0.1, color = "darkgray", size = 3.5)+
  labs(title = "Total Sales per Year", subtitle = "2019-2022", x = "Total Sales (USD)", y = "Year")+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  ##scale_y_discrete(labels = label_number(scale = 1e-2, suffix = "K"))+ reiniciar R y revisar que se haya aplicado
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"))
  
##VENTAS POR AÑO, gráfico de líneas

yearly_sales$year <- as.numeric(as.character(yearly_sales$year))
ggplot(yearly_sales, aes(x = year, y = sales_per_year)) +
  geom_line(color = "skyblue") +
  geom_point(color = "red3", size = 3) +
  geom_text(aes(label = sales_per_year), vjust = -0.5, color = "darkgray", size = 3.5) +
  labs(title = "Total Sales per Year", subtitle = "2019-2022", x = "Year", y = "Total Sales (USD)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_x_continuous(breaks = yearly_sales$year) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"))

##Este gráfico muestra la línea del comportamiento de ventas durante los 4 años
##De ventas de la tienda online. Es adecuado ignorar el valor del 2022, ya que solo
##Se encuentran capturadas las ventas hasta el mes de julio.

  
##VENTAS DE CADA MES POR AÑO

yearly_sales$year <- as.factor(yearly_sales$year)
ggplot(monthly_sales, aes(x = factor(month), y = sales_per_month)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Total Sales per Month", subtitle = "Jan-Dec", x = "Month", y = "Total Sales (USD)",  caption = "Meses con mayor cantidad de ventas para cada año:
    2019, Abril, $1680.7; 2020, Febrero, $1798.3; 2021, Septiembre, $1643.5, 2022, Marzo, $1315.2 ", caption.color = "darkgray") +
  facet_wrap(~ year, ncol = 2)+
  theme_minimal() +
  scale_x_discrete(breaks = seq(1, 12, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 11))



##DESCRIPCIÓN Y ANÁLISIS DEL GRÁFICO
##De acuerdo al resumen de datos se obtuvo que los meses con mayor cantidad de ventas para cada año es:
##2019, Abril, $1680.7; 2020, Febrero, 1798.3; 2021, Septiembre, $1643.5, 2022, Marzo, $1315.2 


##LOS 5 CAFÉS MÁS PEDIDOS
##No la terminé porque los datos son casi iguales y no es un buen gráfico
ggplot(top_5_coffee, aes(x = product_id, y = total_orders))+
  geom_bar(stat = "identity", fill = "lightgoldenrod", width = 0.75)+
  labs(title = "Favorite Coffees", subtitle = "Arabasta-Robusta", x = "Product", y = "Pieces Sold")

##El país que consume más café.

porcentaje_compras <- porcentaje_compras %>%
  mutate(country.x = reorder(country.x, -porcentaje))
ggplot(porcentaje_compras, aes(x = country.x, y = porcentaje))+
  geom_bar(stat = "identity", fill = "tomato")+
  labs(title = "Coffee Consumption by Country", x = "Country", y = "Percentage(%)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  geom_text(aes(label = porcentaje), vjust = -0.1, color = "darkgray", size = 3.5)


##Esta gráfica muestra que el 77.7% de las piezas de café se compran desde Estados Uinidos

##Cuál café genera más ingresos?
##CREAR GRÁFICO CON EL ID DE LOS 5 MÁS RENTABLES, REPRESENTADOS EN PORCENTAJE

ggplot(top_5_percentage, aes(x = reorder(product_id, -percentage_profit), y = percentage_profit)) +
  geom_bar(stat = "identity", fill = "darkseagreen") +
  labs(title = "Most Profitable Products", y = "Profit Percentage", x = "Product ID") +
  geom_text(aes(label = round(percentage_profit, 2)), vjust = -0.2, hjust = 0.5, color = "gray36", size = 3.5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

##Como puede observarse, los 5 productos más vendidos no representan un conjunto de porcentaje mayor al 30%
##Por lo cual, no es conveniente enfocarse en impulsar las ventas por producto, sino por tamaño
##Como es posible observar en las siguientes gráficas

##REVISAR SI HAY DIFERENCIA EN LAS VENTAS SEGÚN EL TAMAÑO DEL CAFÉ

##EL TAMAÑO QUE GENERA MÁS INGRESOS
ggplot(product_orders, aes(y = max_sales, x = factor(size))) +
  geom_bar(stat = "identity", fill = "lightpink2") +
  labs(title = "Sales by Size", y = "Total Sales(USD)", x = "Size") +
  geom_text(aes(label = round(max_sales, 2)), vjust = -0.2, hjust = 0.5, color = "gray36", size = 3.5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


##EL TAMAÑO QUE MÁS SE PIDE

ggplot(product_orders, aes(y = max_quantity, x = factor(size))) +
  geom_bar(stat = "identity", fill = "thistle2", width = 0.7) +
  labs(title = "Orders by Size", y = "Total Orders", x = "Size") +
  geom_text(aes(label = round(max_sales, 2)), vjust = -0.2, hjust = 0.5, color = "gray36", size = 3.5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


##Con estas gráfias podemos observar como el tamaño de café que más se pide
##No es necesariamente el que genera una mayor cantidad de ingresos, por lo que
##Se sugiere aumentar el foco de atención hacia el tamaño de café de 2.5

##Gráfico de dispersión para observar el comportamiento de la demanda vs precio del producto
##El comportamiento de la demanda de cada producto según su precio


ggplot(demanda_precio)+
  geom_point(mapping = aes(x = unit_price, y = total_orders))+
  geom_smooth(method = lm, se = FALSE, aes(x = unit_price, y = total_orders))+
  labs(title = "Price vs Demand", x = "Orders by Product", y = "Price(USD)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


##Tener tarjeta de lealtad marca la diferencia?
##REALIZAR UN GRÁFICO DE CLIENTES CON Y SIN TARJETA DE LEALTAD

ggplot(porcentajes_loyaltycard, aes(x = loyalty_card, y = sales_percentage)) +
  geom_bar(stat = "identity", fill = "lightblue1", width = 0.65) +
  geom_text(aes(label = sales_percentage), color = "gray36", vjust = -0.1)+
  labs(title = "Distribution of Total Purchases",
       x = "Loyalty Card", y = "Sales Percentage(%)") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

##Con este gráfico observamos que el porcentaje d eventas es mayor para los clientes
##sin tarjeta de lealtad, sin embargo el estudio de significancia estadística demostró 
##que la diferencia no es significativa
##Por lo que la información se resume en que, la probabilidad de compra y recompra
##No está influenciada por el hecho de poseer tarjeta de lealtad

##El resultado del análisis se encuentra en un reporte de RMarkDown.


