#INCLUDE <16F887.H>
#USE DELAY(CLOCK=20M)
#FUSES PUT,HS,NOWDT,NOPROTECT,NOLVP

#define LCD_ENABLE_PIN     PIN_D5            
#define LCD_RS_PIN         PIN_D7                                                   
#define LCD_RW_PIN         PIN_D6                         
#define LCD_DATA4          PIN_D4                                                          
#define LCD_DATA5          PIN_C7                                    
#define LCD_DATA6          PIN_C6                                                      
#define LCD_DATA7          PIN_C5   
#include <lcd.c> 


#define MFRC522_CS         PIN_D3                 
#define MFRC522_SCK        PIN_D2
#define MFRC522_SI         PIN_C2                           
#define MFRC522_SO         PIN_D0              
#define MFRC522_RST        PIN_C3    
#include<Built_in.h>

char DATA_KHANH[5] ={0X32, 0XE6, 0X52, 0X06, 0X80};
char DATA_BAO[5]   ={0XF1, 0XC5, 0XA8, 0XD5, 0X49};
char DATA_NHUT[5]  ={0X6C, 0XB0, 0X7B, 0X24, 0X83};

int1 THE_1=0, TAM = 0, i;
char tt_1, tt_2, tt_3;


int1 QUET_THE(char DATA[],char UID[])
{
   FOR (int i = 0; i < 5; i++)
   {
      if(UID[i]== DATA[i] )
      {
         THE_1=1;
      }
      else
      {
         THE_1=0;
         break;
      }
   }
   return THE_1;
}

void bipbip(unsigned int8 hoi,unsigned int8 tieng) 
{ 
   unsigned int8 i, j;
   for(i = 0; i < hoi; i ++){
      for(j = 0; j < tieng; j ++){
         output_high(pin_c0);
         delay_ms(1);
      }
      output_low(pin_c0);
      delay_ms(10);
   }
}

void main()
{

   CHAR UID[6];
   UNSIGNED int TagType;                
   lcd_init ();
   lcd_gotoxy(0,1);
   printf (LCD_PUTC, "HE THONG MO CUA");
   lcd_gotoxy(6,2);
   printf (LCD_PUTC, "NHOM 10");
   delay_ms(3000);
   printf (LCD_PUTC, "\f  Initializing"); 
   MFRC522_Init ();
   delay_ms(100);
   printf (LCD_PUTC, "\n*****Done!******"); 
   delay_ms(1000);
   WHILE (true)
   {
      printf (LCD_PUTC,"\fXin moi quet the");
      IF (MFRC522_isCard (&TagType)) //Check any card
      {                                           
         //Read ID 
         IF (MFRC522_ReadCardSerial (&UID))             
         {
            tt_1 = QUET_THE(DATA_KHANH,UID);
            tt_2 = QUET_THE(DATA_BAO,UID);
            tt_3 = QUET_THE(DATA_NHUT,UID);
            if( tt_1 == 1)
            { 
               if(TAM == 0){
                  printf(LCD_PUTC, "\f Pham Duy Khanh ");
                  lcd_gotoxy(0,2);
                  printf(LCD_PUTC, "xin moi ban vao");
                  bipbip(3,3);
                  delay_ms(100);
                  output_High(PIN_c1);
                  
               }
               else if(TAM == 1){
                  printf(LCD_PUTC, "\f Pham Duy Khanh ");
                  lcd_gotoxy(0,2);
                  printf(LCD_PUTC, "Cua da duoc dong");
                  bipbip(3,3);
                  delay_ms(100);
                  output_low(PIN_c1);
               }
               TAM =~ TAM;
               delay_ms(1000);
            } 
            else if(tt_2 == 1){
               if(TAM == 0){
                  printf(LCD_PUTC, "\f    Phuc Bao    ");
                  lcd_gotoxy(0,2);
                  printf(LCD_PUTC, "xin moi ban vao");
                  bipbip(3,3);
                  delay_ms(100);
                  output_High(PIN_c1);
               
               }
               else if(TAM == 1){
                  printf(LCD_PUTC, "\f    Phuc Bao    ");
                  lcd_gotoxy(0,2);
                  printf(LCD_PUTC, "Cua da duoc dong");
                  bipbip(3,3);
                  delay_ms(100);
                  output_low(PIN_c1);
                  }
                  TAM =~ TAM;
                  delay_ms(1000);
            }
            else if(tt_3 == 1){
               if(TAM == 0){
                  printf(LCD_PUTC, "\f   Minh Nhut    ");
                  lcd_gotoxy(0,2);
                  printf(LCD_PUTC, "xin moi ban vao");
                  bipbip(3,3);
                  delay_ms(100);
                  output_High(PIN_c1);
               
               }
               else if(TAM == 1){
                  printf(LCD_PUTC, "\f   Minh Nhut");
                  lcd_gotoxy(0,2);
                  printf(LCD_PUTC, "Cua da duoc dong");
                  bipbip(3,3);
                  delay_ms(100);
                  output_low(PIN_c1);
               }
               TAM =~ TAM;
               delay_ms(1000);
            }
            else
            {
               lcd_gotoxy(0, 1);
               printf (LCD_PUTC, "The khong hop le");
               lcd_gotoxy(4, 2);
               printf (LCD_PUTC, "WARNING!!!");
               bipbip(10,10);
            } 
         }                                      
         
        MFRC522_Halt () ;
      }    
   }
}
