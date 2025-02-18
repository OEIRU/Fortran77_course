import pandas as pd

# Чтение данных из файла
data = []
with open('all_10.txt', 'r') as file:
    lines = file.readlines()

# Обработка данных
current_record = {}
for line in lines:
    line = line.strip()  # Удаляем лишние пробелы и символы перевода строки
    if not line:  # Пропускаем пустые строки
        continue

    if line.startswith("Method:"):
        current_record['Method'] = line.split(":")[1].strip()
    elif line.startswith("Order of method:"):
        current_record['Order of method'] = int(line.split(":")[1].strip())
    elif line.startswith("Function:"):
        current_record['Function'] = line.split(":")[1].strip()
    elif line.startswith("Analytic:"):
        current_record['Analytic'] = float(line.split(":")[1].strip())
    elif line.startswith("N:"):
        current_record['N'] = int(line.split(":")[1].strip())
    elif line.startswith("Numeric:"):
        current_record['Numeric'] = float(line.split(":")[1].strip())
    elif line.startswith("Ratio Error:"):
        current_record['Ratio Error'] = float(line.split(":")[1].strip())
    elif line.startswith("Estimated Ratio:"):
        current_record['Estimated Ratio'] = float(line.split(":")[1].strip())
    elif line.startswith("Error:"):
        current_record['Error'] = float(line.split(":")[1].strip())
    elif line.startswith("Runge Error:"):
        current_record['Runge Error'] = float(line.split(":")[1].strip())
    elif line.startswith("Richardson:"):
        current_record['Richardson'] = float(line.split(":")[1].strip())
    elif line.startswith("Richardson error:"):
        current_record['Richardson error'] = float(line.split(":")[1].strip())
        data.append(current_record)
        current_record = {}  # Сбрасываем текущую запись

# Создание DataFrame
df = pd.DataFrame(data)

# Экспорт в Excel
df.to_excel('output_10.xlsx', index=False)