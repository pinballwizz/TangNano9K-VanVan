-----------------------------------------------------------------------------------
-- A simulation model of Pacman hardware
-- Copyright (c) MikeJ - January 2006
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- You are responsible for any legal issues arising from your use of this code.
--
-- The latest version of this file can be found at: www.fpgaarcade.com
--
-- Email pacman@fpgaarcade.com
--
-- Revision list
--
-- version 005 Papilio release by Jack Gassett
-- version 004 spartan3e release
-- version 003 Jan 2006 release, general tidy up
-- version 002 optional vga scan doubler
-- version 001 initial release
----------------------------------------------------------------------------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

--library UNISIM;
--  use UNISIM.Vcomponents.all;

--use work.pkg_pacman.all;
----------------------------------------------------------------------------------
entity PACMAN is
	generic(
		eight_sprites : boolean := true
	);
  port (
    O_VIDEO_R             : out std_logic_vector(3 downto 0);
    O_VIDEO_G             : out std_logic_vector(3 downto 0);
    O_VIDEO_B             : out std_logic_vector(3 downto 0);
    O_HSYNC               : out std_logic;
    O_VSYNC               : out std_logic;
    O_AUDIO_L             : out std_logic;
    O_AUDIO_R             : out std_logic;
    I_RESET               : in std_logic;
    CLK_24MHz             : in std_logic;
    SW_FIRE            	  : in std_logic;
    SW_LEFT            	  : in std_logic;
    SW_RIGHT         	  : in std_logic;	
    SW_UP           	  : in std_logic;	
    SW_DOWN         	  : in std_logic;
    I_COIN1               : in std_logic;
    I_1P_START            : in std_logic;
    I_2P_START            : in std_logic;
    AD                    : out std_logic_vector(15 downto 0)
    );
end;
---------------------------------------------------------------------------------
architecture RTL of PACMAN is

	--constant HWSEL_PACMANICMINERMAN : boolean := false ; -- p2 joystick right used for jump, collides with default config.
    signal I_SW           : std_logic_vector(3 downto 0); -- active high
    signal O_LED          : std_logic_vector(2 downto 0);
    signal OSC_IN         : std_logic;
																			
    
	 signal I_RESET_L        : std_logic;
    signal reset            : std_logic;
    signal clk_ref          : std_logic;
    signal clk              : std_logic;
    signal ena_12           : std_logic;
    signal ena_6            : std_logic;
--    signal ena_4            : std_logic;
	 
    -- timing
    signal hcnt             : std_logic_vector(8 downto 0) := "010000000"; -- 80
    signal vcnt             : std_logic_vector(8 downto 0) := "011111000"; -- 0F8

    signal do_hsync         : boolean;
    signal hsync            : std_logic;
    signal vsync            : std_logic;
    signal hblank           : std_logic;
    signal vblank           : std_logic := '1';
    signal h1_inv           : std_logic;
    signal comp_sync_l      : std_logic;
	 signal sprite_xy_data   : std_logic_vector(7 downto 0);
	 
    -- cpu
    signal cpu_ena          : std_logic;
    signal cpu_m1_l         : std_logic;
    signal cpu_mreq_l       : std_logic;
    signal cpu_iorq_l       : std_logic;
    signal cpu_rd_l         : std_logic;
    signal cpu_wr_l         : std_logic;
    signal cpu_rfsh_l       : std_logic;
    signal cpu_halt_l       : std_logic;
    signal cpu_wait_l       : std_logic;
    signal cpu_int_l        : std_logic;
    signal cpu_nmi_l        : std_logic;
    signal cpu_busrq_l      : std_logic;
    signal cpu_busak_l      : std_logic;
    signal cpu_addr         : std_logic_vector(15 downto 0);
    signal cpu_data_out     : std_logic_vector(7 downto 0);
    signal cpu_data_in      : std_logic_vector(7 downto 0);

    signal program_rom_dinl : std_logic_vector(7 downto 0);
    signal program_rom_dinh : std_logic_vector(7 downto 0);
    signal sync_bus_cs_l    : std_logic;

    signal control_reg      : std_logic_vector(7 downto 0);
    --
 	 signal hp               : std_logic_vector ( 4 downto 0);
	 signal vp               : std_logic_vector ( 4 downto 0);
    signal vram_addr        : std_logic_vector(11 downto 0);
	 signal vram_addr_ab     : std_logic_vector(11 downto 0);
    signal ab               : std_logic_vector(11 downto 0);

    signal sync_bus_db      : std_logic_vector(7 downto 0);
    signal sync_bus_r_w_l   : std_logic;
    signal sync_bus_wreq_l  : std_logic;
    signal sync_bus_stb     : std_logic;

    signal cpu_vec_reg      : std_logic_vector(7 downto 0);
    signal sync_bus_reg     : std_logic_vector(7 downto 0);

    signal vram_l           : std_logic;
    signal rams_data_out    : std_logic_vector(7 downto 0);
    -- more decode
    signal wr0_l            : std_logic;
    signal wr1_l            : std_logic;
    signal wr2_l            : std_logic;
    signal iodec_out_l      : std_logic;
    signal iodec_wdr_l      : std_logic;
    signal iodec_in0_l      : std_logic;
    signal iodec_in1_l      : std_logic;
    signal iodec_dipsw_l    : std_logic;

	signal iodec_spr_l      : std_logic;
	signal iodec_sn1_l      : std_logic;
	signal iodec_sn2_l      : std_logic;
	signal iodec_dipsw1_l   : std_logic;
	signal iodec_dipsw2_l   : std_logic;

    -- watchdog
    signal watchdog_cnt     : std_logic_vector(3 downto 0);
    signal watchdog_reset_l : std_logic;

    -- ip registers
    signal button_in        : std_logic_vector(15 downto 0);
    signal buttons	       : std_logic_vector(15 downto 0);
    signal button_debounced : std_logic_vector(15 downto 0);
    signal in0_reg          : std_logic_vector(7 downto 0);
    signal in1_reg          : std_logic_vector(7 downto 0);
    signal dipsw1_reg       : std_logic_vector(7 downto 0);
	 signal dipsw2_reg       : std_logic_vector(7 downto 0);
    signal joystick_reg     : std_logic_vector(5 downto 0);
	 signal joystick2_reg    : std_logic_vector(5 downto 0);

    -- scan doubler signals
    signal video_r          : std_logic_vector(2 downto 0);
    signal video_g          : std_logic_vector(2 downto 0);
    signal video_b          : std_logic_vector(1 downto 0);
	 signal blanking         : std_logic_vector(0 downto 0);
    --
    signal video_r_x2       : std_logic_vector(5 downto 0);
    signal video_g_x2       : std_logic_vector(5 downto 0);
    signal video_b_x2       : std_logic_vector(5 downto 0);
    signal hsync_x2         : std_logic;
    signal vsync_x2         : std_logic;
    signal blanking_x2      : std_logic_vector(0 downto 0);
    --
    signal audio            : std_logic_vector(9 downto 0);
    signal audio_pwm        : std_logic;

	 signal sn1_ce    : std_logic;
	 signal sn2_ce    : std_logic;
	 signal sn1_ready : std_logic;
	 signal sn2_ready : std_logic;
	 signal sn_d      : std_logic_vector(7 downto 0);
	 signal old_we    : std_logic;
--	 signal wav1,wav2 : signed(7 downto 0);
	 signal wav1,wav2 : std_logic_vector(7 downto 0);
-----------------------------------------------------------------------------------	 
begin

  I_RESET_L <= not I_RESET;
  clk       <= CLK_24MHz;
  AD        <= cpu_addr; -- debug
  
-----------------------------------------------------------------------------------  
  u_clocks : entity work.PACMAN_CLOCKS
    port map (
      I_CLK      => CLK_24MHz,
      I_RESET_L  => I_RESET_L,
      O_ENA_12   => ena_12,
      O_ENA_6    => ena_6,
      O_RESET    => reset
      );
-----------------------------------------------------------------------------------  
p_hvcnt : process
begin
	wait until rising_edge(clk);
	if (ena_6 = '1') then
		if hcnt = "111111111" then
			hcnt <= "010000000"; -- 080
		else
			hcnt <= hcnt +"1";
		end if;
		-- hcnt 8 on circuit is 256H_L
		if do_hsync then
			if vcnt = "111111111" then
				vcnt <= "011111000"; -- 0F8
			else
				vcnt <= vcnt +"1";
			end if;
		end if;
	end if;
end process;

vsync <= not vcnt(8);
do_hsync <= (hcnt = "010101111"); -- 0AF

p_sync : process
begin
	wait until rising_edge(clk);
	if (ena_6 = '1') then

		if (hcnt = "010001111") and not eight_sprites then -- 08F
			hblank <= '1';
		elsif (hcnt = "011101111") and not eight_sprites then
			hblank <= '0'; -- 0EF
		elsif (hcnt = "111111111") and eight_sprites then
			hblank <= '1';
		elsif (hcnt = "011111111") and eight_sprites then
			hblank <= '0';
		end if;

		if do_hsync then
			hsync <= '1';
		elsif (hcnt = "011001111") then -- 0CF
			hsync <= '0';
		end if;

		if do_hsync then
			if (vcnt = "111101111") then -- 1EF
				vblank <= '1';
			elsif (vcnt = "100001111") then -- 10F
				vblank <= '0';
			end if;
		end if;
	end if;
end process;

  p_comp_sync : process(hsync, vsync)
  begin
    comp_sync_l <= (not vsync) and (not hsync);
  end process;
--------------------------------------------------------------------------------------------
  -- cpu

p_irq_req_watchdog : process
	variable rising_vblank : boolean;
begin
	wait until rising_edge(clk);
	if (ena_6 = '1') then
		rising_vblank := do_hsync and (vcnt = "111101111"); -- 1EF

		if (control_reg(0) = '0') then
        cpu_nmi_l <= '1';
		elsif rising_vblank then -- 1EF
        cpu_nmi_l <= '0';
		end if;

		-- watchdog 8c
		-- note sync reset
		if (reset = '1') then
			watchdog_cnt <= "1111";
		elsif (iodec_wdr_l = '0') then
			watchdog_cnt <= "0000";
		elsif rising_vblank then
			watchdog_cnt <= watchdog_cnt + "1";
		end if;

		--watchdog_reset_l <= not reset;

		watchdog_reset_l <= '1';
		if (watchdog_cnt = "1111") then
			watchdog_reset_l <= '0';
		end if;
		
		watchdog_reset_l <= not reset;
		
	end if;
end process;
-------------------------------------------------------------------------------------
u_cpu : entity work.T80sed
port map
(
	RESET_n => watchdog_reset_l, -- and (not reset),
	CLK_n   => clk,
	CLKEN   => hcnt(0) and ena_6,
	WAIT_n  => sync_bus_wreq_l,
	INT_n   => '1',
	NMI_n   => cpu_nmi_l,
	BUSRQ_n => '1',
	M1_n    => cpu_m1_l,
	MREQ_n  => cpu_mreq_l,
	IORQ_n  => cpu_iorq_l,
	RD_n    => cpu_rd_l,
	WR_n    => cpu_wr_l,
	RFSH_n  => cpu_rfsh_l,
	HALT_n  => open,
	BUSAK_n => open,
	A       => cpu_addr,
	DI      => cpu_data_in,
	DO      => cpu_data_out
);
-------------------------------------------------------------------------------------
  -- primary addr decode
  --
-- rom     0x0000 - 0x3FFF
-- syncbus 0x4000 - 0x7FFF
sync_bus_cs_l   <= '0' when cpu_mreq_l = '0' and cpu_rfsh_l = '1' and cpu_addr(14) = '1' else '1';
sync_bus_wreq_l <= '0' when sync_bus_cs_l = '0' and hcnt(1) = '1' and cpu_rd_l = '0' else '1';
sync_bus_stb    <= '0' when sync_bus_cs_l = '0' and hcnt(1) = '0' else '1';
sync_bus_r_w_l  <= '0' when sync_bus_stb  = '0' and cpu_rd_l = '1' else '1';

--
-- sync bus custom ic
--
p_sync_bus_reg : process
begin
	wait until rising_edge(clk);
	if (ena_6 = '1') then
		-- register on sync bus module that is used to store interrupt vector
		if (cpu_iorq_l = '0') and (cpu_m1_l = '1') then
			cpu_vec_reg <= cpu_data_out;
		end if;

		-- read holding reg
		if (hcnt(1 downto 0) = "01") then
			sync_bus_reg <= cpu_data_in;
		end if;
	end if;
end process;

  p_vram_comb : process(hcnt, cpu_addr, sync_bus_stb)
    variable a,b : std_logic;
  begin

    a := not (cpu_addr(12) or sync_bus_stb);
    b := hcnt(1) and hcnt(0);
    vram_l <= not (a or b);
  end process;

-- WRITE
-- out_l 0x5000 - 0x503F control space
-- spr_l 0x5060 - 0x506F sprite
-- wdr_l 0x50C0 - 0x50FF watchdog reset
iodec_out_l <= '0' when sync_bus_r_w_l = '0' and cpu_addr(15 downto 6) = X"50"&"00" else '1';
iodec_spr_l <= '0' when sync_bus_r_w_l = '0' and cpu_addr(15 downto 4) = X"50"&X"6" else '1';
iodec_wdr_l <= '0' when sync_bus_r_w_l = '0' and cpu_addr(15 downto 6) = X"50"&"11" else '1';

-- READ
-- in0_l   0x5000 - 0x503F in port 0
-- in1_l   0x5040 - 0x507F in port 1
-- dipsw_l 0x5080 - 0x50BF dip switches
iodec_in0_l    <= '0' when sync_bus_r_w_l = '1' and cpu_addr(15 downto 6) = X"50"&"00" else '1';
iodec_in1_l    <= '0' when sync_bus_r_w_l = '1' and cpu_addr(15 downto 6) = X"50"&"01" else '1';
iodec_dipsw1_l <= '0' when sync_bus_r_w_l = '1' and cpu_addr(15 downto 6) = X"50"&"10" else '1';
iodec_dipsw2_l <= '0' when sync_bus_r_w_l = '1' and cpu_addr(15 downto 6) = X"50"&"11" else '1';

p_control_reg : process
begin
	-- 8 bit addressable latch 7K
	-- (made into register)

	-- 0 interrupt ena
	-- 1 sound ena
	-- 2 not used
	-- 3 flip
	-- 4 1 player start lamp
	-- 5 2 player start lamp
	-- 6 coin lockout
	-- 7 coin counter

	wait until rising_edge(clk);
	if (ena_6 = '1') then
		if (watchdog_reset_l = '0') then
			control_reg <= (others => '0');
		elsif (iodec_out_l = '0') then
			control_reg(to_integer(unsigned(cpu_addr(2 downto 0)))) <= cpu_data_out(0);
		end if;
	end if; 
end process;

  p_db_mux_comb : process(hcnt, cpu_data_out, rams_data_out)
  begin
    -- simplified data source for video subsystem
    -- only cpu or ram are sources of interest
    if (hcnt(1) = '0') then
      sync_bus_db <= cpu_data_out;
    else
      sync_bus_db <= rams_data_out;
    end if;
  end process;

cpu_data_in <=	cpu_vec_reg      when (cpu_iorq_l = '0') and (cpu_m1_l = '0') else 
					sync_bus_reg     when sync_bus_wreq_l = '0' else
					program_rom_dinl when cpu_addr(15 downto 14) = "00" else -- ROM at 0000 - 3fff
					program_rom_dinh when cpu_addr(15 downto 14) = "10" else -- ROM at 8000 - bfff
					in0_reg          when iodec_in0_l = '0' else
					in1_reg          when iodec_in1_l = '0' else
					dipsw1_reg       when iodec_dipsw1_l = '0' else
					dipsw2_reg       when iodec_dipsw2_l = '0' else
					rams_data_out;
--------------------------------------------------------------------------------------
  u_rams : entity work.PACMAN_RAMS
    port map (
      -- note, we get a one clock delay from our rams
      I_AB     => ab,
      I_DATA   => cpu_data_out, -- cpu only source of ram data
      O_DATA   => rams_data_out,
      I_R_W_L  => sync_bus_r_w_l,
      I_VRAM_L => vram_l,
      ENA_6    => ena_6,
      CLK      => clk
      );
--------------------------------------------------------------------------------------
  -- example of internal program rom, if you have a big enough device
  u_program_rom0 : entity work.ROM_PGM_0
    port map (
      CLK         => clk,
--      ENA         => ena_6,
      ADDR        => cpu_addr(13 downto 0),
      DATA        => program_rom_dinl
      );

--	Commented out for use on the Papilio board so ROMs can be merged into the bitstream. Not enough BRAM to include this for the Papilio One.
  u_program_rom1 : entity work.ROM_PGM_1
    port map (
      CLK         => clk,
--      ENA         => ena_6,
      ADDR        => cpu_addr(13 downto 0),
      DATA        => program_rom_dinh
      );
---------------------------------------------------------------------------------------
-- vram addr custom ic
hp <= hcnt(7 downto 3) when control_reg(3) = '0' else not hcnt(7 downto 3);
vp <= vcnt(7 downto 3) when control_reg(3) = '0' else not vcnt(7 downto 3);
vram_addr <= '0' & hcnt(2) & vp & hp when hcnt(8)='1' else
             x"FF" & hcnt(6 downto 4) & hcnt(2) when hblank = '1' else
             '0' & hcnt(2) & hp(3) & hp(3) & hp(3) & hp(3) & hp(0) & vp;

ab <= cpu_addr(11 downto 0) when hcnt(1) = '0' else vram_addr;

----------------------------------------------------------------------------------------
  -- Sprites

	sprite_xy_ram : entity work.dpram2 generic map (4,8)
	port map
	(
		clock_a   => CLK,
		enable_a  => ENA_6,
		wren_a    => not iodec_spr_l,
		address_a => AB(3 downto 0),
		data_a    => sync_bus_db,

		clock_b   => CLK,
		address_b => AB(3 downto 0),
		q_b       => sprite_xy_data
	);
-------------------------------------------------------------------------------------------
--  sprite_xy_ram : for i in 0 to 7 generate
  -- should be a latch, but we are using a clock
  -- ops are disabled when ME_L is high or WE_L is low
--  begin
--    inst: RAM16X1D
--      port map (
--        a0    => AB(0),
--        a1    => AB(1),
--        a2    => AB(2),
--        a3    => AB(3),
--        dpra0 => AB(0),
--        dpra1 => AB(1),
--        dpra2 => AB(2),
--        dpra3 => AB(3),
--        wclk  => CLK,
--        we    => not iodec_spr_l,
--        d     => sync_bus_db(i),
--        dpo   => sprite_xy_data(i)
--        );
--  end generate;
--------------------------------------------------------------------------------------------
  -- video subsystem
  --
  u_video : entity work.PACMAN_VIDEO
    port map (
      I_HCNT        => hcnt,
      I_VCNT        => vcnt,
      --
      sprite_xy     => sprite_xy_data,
      vram_data     => sync_bus_db,
      --
      I_HBLANK      => hblank,
      I_VBLANK      => vblank,
      I_FLIP        => control_reg(3),
      --
      O_RED         => video_r,
      O_GREEN       => video_g,
      O_BLUE        => video_b,
--      O_BLANKING    => blanking,
      --
      ENA_6         => ena_6,
      CLK           => clk
      );
-------------------------------------------------------------------------------------------
  u_dblscan : entity work.scandoubler
    port map (
		clk_sys => clk,
		r_in => video_r & video_r,
		g_in => video_g & video_g,
		b_in => video_b & video_b & video_b,
		hs_in => not hsync,
		vs_in => vsync,
		r_out => video_r_x2,
		g_out => video_g_x2,
		b_out => video_b_x2,
		hs_out => hsync_x2,
		vs_out => vsync_x2,
		scanlines => "00"
	);
--------------------------------------------------------------------------------------------
      O_VIDEO_R <= video_r_x2(5 downto 2);
      O_VIDEO_G <= video_g_x2(5 downto 2);
      O_VIDEO_B <= video_b_x2(5 downto 2);
      O_HSYNC   <= hSync_X2;
      O_VSYNC   <= vSync_X2;
--------------------------------------------------------------------------------------------
  -- audio subsystem

process(clk, reset) begin
	if reset = '1' then
		sn1_ce <= '1';
		sn2_ce <= '1';
	elsif rising_edge(clk) then

		if sn1_ready = '1' then
			sn1_ce <= '1';
		end if;

		if sn2_ready = '1' then
			sn2_ce <= '1';
		end if;

		old_we <= cpu_wr_l;
		if old_we = '1' and cpu_wr_l = '0' and cpu_iorq_l = '0' then
			if cpu_addr(7 downto 0) = X"01" then
				sn1_ce <= '0';
				sn_d <= cpu_data_out;
			end if;
			if cpu_addr(7 downto 0) = X"02" then
				sn2_ce <= '0';
				sn_d <= cpu_data_out;
			end if;
		end if;
	end if;
end process;
--------------------------------------------------------------------------------------------
sn1 : entity work.sn76489_top
generic map (
	clock_div_16_g => 1
)
port map (
	clock_i    => clk,
	clock_en_i => ena_6, -- ena_4,
	res_n_i    => not RESET,
	ce_n_i     => sn1_ce,
	we_n_i     => sn1_ce,
	ready_o    => sn1_ready,
	d_i        => sn_d,
	aout_o     => wav1
);
--------------------------------------------------------------------------------------------
sn2 : entity work.sn76489_top
generic map (
	clock_div_16_g => 1
)
port map (
	clock_i    => clk,
	clock_en_i => ena_6, -- ena_4,
	res_n_i    => not RESET,
	ce_n_i     => sn2_ce,
	we_n_i     => sn2_ce,
	ready_o    => sn2_ready,
	d_i        => sn_d,
	aout_o     => wav2
);
----------------------------------------------------------------------------------------
	audio <= (wav1 & "00") + (wav2 & "00");
----------------------------------------------------------------------------------------
  -- Audio DAC
  
  u_dac : entity work.dac
    generic map(
      msbi_g => 9
    )
    port  map(
      clk_i   => ena_12,
      res_n_i => I_RESET_L,
      dac_i   => audio,
      dac_o   => audio_pwm
    );

  O_AUDIO_L <= audio_pwm;
  O_AUDIO_R <= audio_pwm;
-----------------------------------------------------------------------------------------
  p_input_registers : process
  begin
    wait until rising_edge(clk);
    if (ena_6 = '1') then
		-- on is low
      in0_reg(7) <= '1'; -- button_debounced(15);		-- coin2 
      in0_reg(6) <= '1';		  				   -- ? 
      in0_reg(5) <= not I_COIN1; -- button_debounced(8);		-- coin1		
      in0_reg(4) <= not SW_FIRE; -- button_debounced(4);     -- p1 fire
      in0_reg(3) <= not SW_DOWN; -- button_debounced(1); 		-- p1 down
      in0_reg(2) <= not SW_RIGHT; -- button_debounced(3); 		-- p1 right
      in0_reg(1) <= not SW_LEFT; -- button_debounced(2); 		-- p1 left
      in0_reg(0) <= not SW_UP; -- button_debounced(0); 		-- p1 up

      in1_reg(7) <= '1';                     -- Table 1=Up / 0=Coskctail
      in1_reg(6) <= not I_2P_START; -- button_debounced(7); 		-- start2 
      in1_reg(5) <= not I_1P_START; -- button_debounced(6); 		-- start1 
      in1_reg(4) <= not SW_FIRE; -- button_debounced(13);    -- p1 fire    					
      in1_reg(3) <= not SW_DOWN; -- button_debounced(10);		-- p2 down
      in1_reg(2) <= not SW_RIGHT; -- button_debounced(12); 	-- p2 right
      in1_reg(1) <= not SW_LEFT; -- button_debounced(11); 	-- p2 left
      in1_reg(0) <= not SW_UP; -- button_debounced(9); 		-- p2 up		

	   -- on is 0      
      dipsw1_reg(7) <= '1'; -- character set ?
      dipsw1_reg(6) <= '1'; -- (Hz VGA modo Cocktail? Con 0 en modo cocktail VGA va mal)
      dipsw1_reg(5 downto 4) <= "11"; -- vidas
      dipsw1_reg(3 downto 2) <= "10"; -- bonus?
      dipsw1_reg(1 downto 0) <= "00"; -- flip & cocktail/up
	  
      dipsw2_reg <= "00000000"; -- Coins Setup

   end if;
  end process;
------------------------------------------------------------------------------------------
end RTL;