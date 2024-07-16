int n;
int a[10] = {4,2,3,1,5,6,9,8,7,0};
int bubblesort(int arr[])
{
    int i;
    int j;
    i =0; 
    while(i < n-1){
    // Last i elements are already in place
        j = 0;
        while(j < n-i-1){
            if (arr[j] > arr[j+1]) {
                // swap(&arr[j], &arr[j+1]); 
                int tmp;
                tmp = arr[j+1];
                arr[j+1] = arr[j];
                arr[j] = tmp;
            }
            j = j + 1;
        }
        i = i + 1;
    }
    return 0;
}

int main(){
    n = 10;
    int i;
    i = bubblesort(a);
    while (i < n) {
        int tmp;
        tmp = a[i];
        putint(tmp);
        tmp = 10;
        putch(tmp);
        i = i + 1;
    }
    return 0;
}