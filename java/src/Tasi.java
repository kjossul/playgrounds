import java.util.HashMap;
import java.util.Scanner;

public class Tasi {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Categoria?");
        int categoria = scanner.nextInt();
        System.out.println("Rendita?");
        double rendita = scanner.nextDouble();

        double imposta = calcolaImposta(categoria, rendita);
        System.out.println("L'imposta è " + imposta + "€");
    }

    private static double calcolaImposta(int categoria, double rendita) {
        HashMap<Integer, Integer> moltiplicatore = new HashMap<>();
        moltiplicatore.put(1, 160);
        moltiplicatore.put(2, 140);
        moltiplicatore.put(3, 65);

        HashMap<Integer, Double> aliquota = new HashMap<>();
        aliquota.put(1, calcolaAliquotaCasa(rendita));
        aliquota.put(2, 0.1 / 100);
        aliquota.put(3, 0.21 / 100);

        int detrazioni = calcolaDetrazioni(categoria, rendita);
        double baseImponibile = moltiplicatore.get(categoria) * rendita * 1.05;
        return baseImponibile * aliquota.get(categoria) - detrazioni;
    }

    private static int calcolaDetrazioni(int categoria, double rendita) {
        if (categoria != 1)
            return 0;
        else {
            Scanner scanner = new Scanner(System.in);
            System.out.println("Numero figli?");
            int figli = scanner.nextInt();

            int detrazioni = 0;
            if (rendita > 300 && rendita <= 400)
                detrazioni = 90;
            else if (rendita > 400 && rendita <= 500)
                detrazioni= 60;
            else if (rendita > 500 && rendita <= 900)
                detrazioni = 30;

            return detrazioni + 5 * figli;
        }
    }

    private static double calcolaAliquotaCasa(double rendita) {
        if (rendita <= 300)
            return 0;
        else if (rendita <= 600)
            return 0.25 / 100;
        else
            return 0.33 / 100;
    }
}
