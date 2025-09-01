copy /b van-20.18 + van-21.19 gfx1.bin
make_vhdl_prom gfx1.bin GFX1.vhd

copy /b van-1.50 + van-2.51 + van-3.52 + van-4.53 main.bin
make_vhdl_prom main.bin ROM_PGM_0.vhd

copy /b van-5.39 + van-5.39 + van-5.39 + van-5.39 main1.bin
make_vhdl_prom main1.bin ROM_PGM_1.vhd

make_vhdl_prom 6331-1.6 PROM1_DST.vhd

pause