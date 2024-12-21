# Makefile для проекта с операциями с матрицами и векторами

# Переменные
FC = gfortran            # Компилятор Fortran
FFLAGS = -O2             # Флаги компилятора (например, оптимизация)
EXEC = convecter_to_txt converter_matrix_to_bin converter_vector_to_bin mult_bin mult_txt  # Имя исполняемых файлов

# Исходные файлы
SRC = convecter_to_txt.f90 converter_matrix_to_bin.f90 converter_vector_to_bin.f90 mult_bin.f90 mult_txt.f90

# Цели по умолчанию
all: $(EXEC)

# Правило для создания исполняемых файлов
$(EXEC): $(SRC)
	$(FC) $(FFLAGS) $(SRC) -o $@

# Удаление временных и скомпилированных файлов
clean:
	rm -f $(EXEC) *.o

# Дополнительные цели

# Цель для компиляции и запуска конвертации матрицы в текстовый формат
convert_matrix_to_txt:
	$(FC) $(FFLAGS) convecter_to_txt.f90 -o convecter_to_txt

# Цель для компиляции и запуска конвертации матрицы в бинарный формат
convert_matrix_to_bin:
	$(FC) $(FFLAGS) converter_matrix_to_bin.f90 -o converter_matrix_to_bin

# Цель для компиляции и запуска конвертации вектора в бинарный формат
convert_vector_to_bin:
	$(FC) $(FFLAGS) converter_vector_to_bin.f90 -o converter_vector_to_bin

# Цель для компиляции и запуска умножения матрицы на вектор с бинарными данными
multiply_bin:
	$(FC) $(FFLAGS) mult_bin.f90 -o mult_bin

# Цель для компиляции и запуска умножения матрицы на вектор с текстовыми данными
multiply_txt:
	$(FC) $(FFLAGS) mult_txt.f90 -o mult_txt

# Печать целей
help:
	@echo "Список доступных целей:"
	@echo "  all                   - Скомпилировать все программы"
	@echo "  clean                 - Удалить скомпилированные файлы"
	@echo "  convert_matrix_to_txt - Компиляция и запуск конвертации матрицы в текстовый формат"
	@echo "  convert_matrix_to_bin - Компиляция и запуск конвертации матрицы в бинарный формат"
	@echo "  convert_vector_to_bin - Компиляция и запуск конвертации вектора в бинарный формат"
	@echo "  multiply_bin          - Компиляция и запуск умножения матрицы на вектор с бинарными данными"
	@echo "  multiply_txt          - Компиляция и запуск умножения матрицы на вектор с текстовыми данными"
