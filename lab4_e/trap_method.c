#include <stdio.h>
#include <math.h>

// Функция для интегрирования
double f(double x) { return pow(x,5); }

// Метод трапеций
double trapezoidal_rule(double (*func)(double), double a, double b, int n) {
    double h = (b - a) / n;
    double result = 0.5 * (func(a) + func(b));
    for (int i = 1; i < n; i++) {
        result += func(a + i * h);
    }
    return result * h;
}

// Аналитическое значение интеграла
double analytic_value(double a, double b) {
    return 1/6;
}

// Основная функция
int main() {
    double A = 0, B = 1; // Границы интегрирования
    int max_steps = 25;   // Максимальное количество итераций (2^n)

    double analytic_val = analytic_value(A, B);

    printf("Число отрезков\tШаг\tЧисленное значение\tОтношение погрешностей\tПогрешность\tОценка по Рунге\tУточнение по Ричардсону\tПогрешность уточненного решения\n");

    double prev_integral = 0, current_integral = 0, runge_error = 0, richardson_correction = 0;
    double prev_error = 0;

    for (int n = 0; n <= max_steps; n++) {
        int N = pow(2, n); // Количество отрезков
        double h = (B - A) / N; // Шаг
        current_integral = trapezoidal_rule(f, A, B, N);

        double error = fabs(current_integral - analytic_val);
        double runge_ratio = (n > 0) ? (error / prev_error) : 0;

        if (n > 0) {
            runge_error = fabs(current_integral - prev_integral) / 3.0; // p = 2 для метода трапеций
            richardson_correction = current_integral + runge_error;
        } else {
            runge_error = 0;
            richardson_correction = current_integral;
        }

        double corrected_error = fabs(richardson_correction - analytic_val);

        printf("%d\t%.6f\t%.8e\t%.8e\t%.8e\t%.8e\t%.8e\t%.8e\n",
               N, h, current_integral, runge_ratio, error, runge_error, richardson_correction, corrected_error);

        prev_integral = current_integral;
        prev_error = error;
    }

    return 0;
}