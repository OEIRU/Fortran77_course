import matplotlib.pyplot as plt

# Новые данные
values = [
    6.18619E-12, 1.9296E-12, 5.06345E-13, 1.28092E-13, 3.20854E-14,
    8.02136E-15, 1.97065E-15, 5.27356E-16, 1.94289E-16, 1.66533E-16,
    8.32667E-17, 4.16334E-16, 5.55112E-17, 2.498E-16, 3.88578E-16,
    1.38778E-16, 1.02696E-15, 6.66134E-16, 9.71445E-16, 1.41553E-15,
    6.55032E-15, 6.02296E-15, 2.00673E-14, 1.44051E-14, 1.92346E-14,
    9.15934E-15, 2.91434E-15, 1.01419E-13, 3.33067E-15, 6.43929E-14
]

# Индексы для оси X
x = range(1, len(values) + 1)

# Построение графика
plt.figure(figsize=(10, 6))
plt.semilogy(x, values, marker='o')  # Логарифмическая ось Y
plt.title("Логарифмический график новых значений")
plt.xlabel("Индекс")
plt.ylabel("Значение")
plt.grid(True)
plt.xticks(x)  # Устанавливаем метки по оси X
plt.show()
