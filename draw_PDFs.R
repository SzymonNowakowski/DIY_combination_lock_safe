source("digits.R")

pixel_cnt <- nrow(digits[["one"]])

#all sizes in mm
feather_width_mm <- 10
plywood_thickness_mm <- 3

# the below specs must be even multiplicities of feather length
case_width_in_feathers <- 26
case_depth_in_feathers <- 16
base_height_in_feathers <- 8
lid_height_in_feathers <- 2

h_mm <- 20
r_mm <- sqrt(h_mm^2 / (1 - 4 * sin(2*pi/20)^2))
a_mm <- sqrt(r_mm^2 - h_mm^2)
dial_pixel_protrudes_max_mm <- (a_mm / pixel_cnt) * .7  
dial_opening_depth <- 2 * sqrt((r_mm + dial_pixel_protrudes_max_mm)^2 - h_mm^2) + 1  #one additional mm just to be on a safe side
dial_opening_width <- 46 # room for 3 sets of four dials (and one separator) 3 mm each (3*5*3) plus one additional mm just to be on a safe side 




if (feather_width_mm <= plywood_thickness_mm)
  stop("Feather length must be greater than plywood thickness")

if (case_depth_in_feathers %% 2 != 0 | case_width_in_feathers %% 2 !=0 |
    base_height_in_feathers %% 2 != 0 | lid_height_in_feathers %% 2 != 0)
  stop("The dimensions must be even multiplicities of the feather length")

if (case_depth_in_feathers <= 0 | case_width_in_feathers <=0 |
    base_height_in_feathers <= 0 | lid_height_in_feathers <= 0)
  stop("The dimensions must be positive multiplicities of the feather length")


draw_harpoon <- function(from, to, head_len=0.3, head_width=0.3) {
  # shaft
  segments(from[1], from[2], to[1], to[2])
  
  # direction vector
  dx <- to[1] - from[1]
  dy <- to[2] - from[2]
  len <- sqrt(dx^2 + dy^2)
  
  # perpendicular vector
  px <- dy
  py <- -dx
  
  # harpoon side
  segments(to[1], to[2],
           to[1] - head_len*dx + head_width*px,
           to[2] - head_len*dy + head_width*py)
}

# Draw an arc centered at (xc, yc), radius r,
# from angle alpha to beta using only lines().
draw_arc <- function(xc, yc, r, alpha, beta, clockwise=TRUE, n = 64) {
  if (clockwise) {
    if (beta > alpha) beta <- beta - 2*pi
  } else {
    if (beta < alpha) beta <- beta + 2*pi
  }
  th <- seq(alpha, beta, length.out = n)
  x <- xc + r * cos(th)
  y <- yc + r * sin(th)
  lines(x, y)

}

draw_three_separators <- function(x, y) {
  for (i in 1:4) {
    lines(c(x, x + 3), c(y + 3*i, y + 3*i))
    r <- sqrt((dial_opening_depth/2)^2 + h_mm^2)
    angle <- asin(h_mm / r)
    draw_arc(x + 3 + dial_opening_depth/2, y + 3*i - h_mm, r, pi - angle, angle)
    lines(c(x + dial_opening_depth + 3, x + dial_opening_depth + 6), c(y + 3*i, y + 3* i))
  }
  lines(c(x, x), c(y + 3, y + 12))
  lines(c(x + dial_opening_depth + 6, x + dial_opening_depth + 6), c(y + 3, y + 12))
}

draw_dial <- function(x, y, r, pixel_vector=rep(F, pixel_cnt * 10)) {
  if (length(pixel_vector) != pixel_cnt * 10)
    stop("Pixel vector must have 10 times pixel_cnt positions")
  
  draw_harpoon(c(x + cos(2*pi/20) * r * 1/2, y + sin(2*pi/20) * r * 1/2), c(x + cos(2*pi/20) * r * 3/4, y + sin(2*pi/20) * r * 3/4))   # right-sided harpoon pointing at digit 1 to help with alignment of dials during construction
  
  angle <- 0
  current_point <- c(x + cos(angle) * r, y + sin(angle) * r)
  for (side in 1:10) {
    angle = side * (2*pi / 10)
    next_point <- c(x + cos(angle) * r, y + sin(angle) * r)
    #lines(c(current_point[1], next_point[1]), c(current_point[2], next_point[2]))
    #instead of plotting a single line, this line will be divided into 7 regions and the regions will be plotted separately
    region_start <- current_point
    for (region in 1:pixel_cnt) {
      region_end <- current_point + region * (next_point - current_point) / pixel_cnt
      if (pixel_vector[(side - 1) * pixel_cnt + region]) {   #if it is pixeled, we must make a little protruding pixel if it is the first one or it is after non-pixeled, move right-perpendicuraly 20%, then move along, then again: perpendicuraly, but only if this is the last one or after this one, there is a non-pixeled
        # compute the 50% vector along
        
        vector_along <- (region_end - region_start) * 50/100
        right_perpendicular_vector <- c(vector_along[2], -vector_along[1])   #(-b, a) is right-perpendicular to a vector (a,b)

        if (region == 1 || !pixel_vector[(side - 1) * pixel_cnt + region-1]) {  #lazy evaluation of || operator
          part_start <- region_start
          part_end <- region_start + right_perpendicular_vector
          lines(c(part_start[1], part_end[1]), c(part_start[2], part_end[2]))
        } #otherwise,  part_end is as it was in the previous region
        
        if (region == pixel_cnt %/% 2 + 1) {   #middle region - additional 20 % up
          part_start <- part_end
          part_end <- part_end + right_perpendicular_vector * 2/5
          lines(c(part_start[1], part_end[1]), c(part_start[2], part_end[2]))
        }
        
        # the next part goes along
        part_start <- part_end
        part_end <- part_end + 2 * vector_along
        lines(c(part_start[1], part_end[1]), c(part_start[2], part_end[2]))
        
        if (region == pixel_cnt %/% 2 + 1) {   #middle region - additional 20 % down
          part_start <- part_end
          part_end <- part_end - right_perpendicular_vector * 2/5
          lines(c(part_start[1], part_end[1]), c(part_start[2], part_end[2]))
        }
        
        # the return is perpendicular sometimes
        if (region == pixel_cnt || !pixel_vector[(side - 1) * pixel_cnt + region+1]) {  #lazy evaluation of || operator
          part_start <- part_end
          part_end <- part_end - right_perpendicular_vector
          lines(c(part_start[1], part_end[1]), c(part_start[2], part_end[2]))
        }
        
      } else {    
        lines(c(region_start[1], region_end[1]), c(region_start[2], region_end[2]))
      }
        
      region_start <- region_end
    }
      
    current_point <- next_point
    
  }
}

draw_straight_line <- function(segment_cnt, coordinate, current_pos, increase_on_first_feather_side, increase_on_feather_width, smaller_first, smaller_last, skip_feather = FALSE) {
  new_pos <- current_pos
  direction <- 1
  for (i in 1:segment_cnt) {
    if ((smaller_last && i==segment_cnt) | (smaller_first && i==1)) {
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * (feather_width_mm - plywood_thickness_mm)
    } else {
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * feather_width_mm
    }
    
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
  }
  
  if (!skip_feather) {
    #and a final go up or down as if it were a one large feather
    new_pos[3-coordinate] <- current_pos[3-coordinate] + (-1)^(increase_on_first_feather_side + direction) * plywood_thickness_mm
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
  }
  
  return(current_pos)
}


draw_feathered_line <- function(segment_cnt, coordinate, current_pos, increase_on_first_feather_side, increase_on_feather_width, smaller_first, smaller_last) {
  new_pos <- current_pos
  direction <- 1
  for (i in 1:segment_cnt) {
    if ((smaller_last && i==segment_cnt) | (smaller_first && i==1)) {
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * (feather_width_mm - plywood_thickness_mm)
    } else {
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * feather_width_mm
    }
    
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
    
    if (i==segment_cnt) 
      return(current_pos)
    
    new_pos[3-coordinate] <- current_pos[3-coordinate] + (-1)^(increase_on_first_feather_side + direction) * plywood_thickness_mm
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
    direction <- direction + 1
  }
  return(current_pos)
}

draw_top_or_bottom <- function(width_segment_count, depth_segment_count, opening_width_mm=0, opening_depth_mm=0, opening_shift_mm=c(20, 10)) {
  current_pos <- c(0,plywood_thickness_mm)
  
  current_pos <- draw_feathered_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE)
  current_pos <- draw_feathered_line(depth_segment_count,2, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE)
  current_pos <- draw_feathered_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE)
  current_pos <- draw_feathered_line(depth_segment_count,2, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE)
  
  if (opening_width_mm > 0) {
    #position the opening  (sized opening_width by opening depth) in the middle width-wise and with opening_shift_mm shift from 0 depth_wise
    xL <- opening_shift_mm[1]
    xR <- xL + opening_width_mm
    y0 <- opening_shift_mm[2]
    y1 <- y0 + opening_depth_mm
    
    # Narysuj prostokąt otworu 4 komendami lines()
    lines(c(xL, xR), c(y0, y0))  # dół
    lines(c(xL, xR), c(y1, y1))  # góra
    lines(c(xL, xL), c(y0, y1))  # lewa
    lines(c(xR, xR), c(y0, y1))  # prawa
  }
} 



draw_side_wall <- function(width_segment_count, height_segment_count) {
  current_pos <- c(0,0)
  # if the last segment is NOT A TOOTH, we should stop drawing a "plywood thickness" before it is finished,
  # so the next perpendicular not-a-tooth segment can be started with a small decline
  current_pos <- draw_straight_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE)
  current_pos <- draw_feathered_line(height_segment_count,2, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=TRUE, smaller_first=TRUE, smaller_last=FALSE)
  current_pos <- draw_feathered_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE)
  current_pos <- draw_feathered_line(height_segment_count,2, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=FALSE, smaller_first=TRUE, smaller_last=FALSE)
  
} 

draw_inner_back<- function(width_segment_count, height_segment_count) {
  current_pos <- c(plywood_thickness_mm,0)
  current_pos <- draw_straight_line(width_segment_count, 1, current_pos, increase_on_feather_width=TRUE, smaller_first=TRUE, smaller_last=TRUE, skip_feather=TRUE)
  current_pos <- draw_straight_line(height_segment_count,2, current_pos, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE, skip_feather=TRUE)
  current_pos <- draw_straight_line(width_segment_count, 1, current_pos, increase_on_feather_width=FALSE, smaller_first=TRUE, smaller_last=TRUE, skip_feather=TRUE)
  current_pos <- draw_straight_line(height_segment_count,2, current_pos, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE, skip_feather=TRUE)
  
} 

draw_L <- function(x0, y0, L = c(3, 6, 4, 3, 7, 9)) {
  stopifnot(length(L) == 6)
  # points around the outline
  p1 <- c(x0, y0)
  p2 <- p1 + c(L[1],    0)   # right
  p3 <- p2 + c(   0, -L[2])  # down
  p4 <- p3 + c(L[3],    0)  # left
  p5 <- p4 + c(   0, -L[4])  # down
  p6 <- p5 + c(-L[5],    0)   # right
  p7 <- p6 + c(   0,  L[6])  # up
  # draw edges
  lines(c(p1[1], p2[1]), c(p1[2], p2[2]))
  lines(c(p2[1], p3[1]), c(p2[2], p3[2]))
  lines(c(p3[1], p4[1]), c(p3[2], p4[2]))
  lines(c(p4[1], p5[1]), c(p4[2], p5[2]))
  lines(c(p5[1], p6[1]), c(p5[2], p6[2]))
  lines(c(p6[1], p7[1]), c(p6[2], p7[2]))
  # close the loop (final left segment)
  lines(c(p7[1], p1[1]), c(p7[2], p1[2]))
  
}

draw_loop_lines_exact <- function(x0, y0,
                                  len_left = 8, down1 = 3, diag45 = 5, down2 = 5) {
  # Start
  S <- c(x0, y0)
  # 8 mm left
  A <- c(x0 - len_left, y0)
  # 3 mm down
  B <- c(A[1], A[2] - down1)
  # 5 mm at 45° left-down
  off <- diag45 / sqrt(2)
  C <- c(B[1] - off, B[2] - off)
  # 5 mm down
  D <- c(C[1], C[2] - down2)
  # 45° up-right until directly under the start (x == x0)
  dx <- x0 - D[1]                # move this much in x and y
  E  <- c(x0, D[2] + dx)
  
  # draw the polyline: S -> A -> B -> C -> D -> E -> S
  lines(c(S[1], A[1]), c(S[2], A[2]))
  lines(c(A[1], B[1]), c(A[2], B[2]))
  lines(c(B[1], C[1]), c(B[2], C[2]))
  lines(c(C[1], D[1]), c(C[2], D[2]))
  lines(c(D[1], E[1]), c(D[2], E[2]))
  lines(c(E[1], S[1]), c(E[2], S[2]))


}


# Draw narrowingrectangle by top-left corner, width w, height h
draw_narrowing_rectangle <- function(x0, y0, w, h) {
  x1 <- x0 + w; y1 <- y0 - h
  lines(c(x0, x1), c(y0, y0))  # top
  lines(c(x1, x1 - w * 0.2), c(y0, y1))  # right
  lines(c(x1 - w * 0.2, x0 + w * 0.2), c(y1, y1))  # bottom
  lines(c(x0 + w * 0.2, x0), c(y1, y0))  # left
}

# Draw rectangle by top-left corner, width w, height h
draw_rectangle <- function(x0, y0, w, h) {
  x1 <- x0 + w; y1 <- y0 - h
  lines(c(x0, x1), c(y0, y0))  # top
  lines(c(x1, x1), c(y0, y1))  # right
  lines(c(x1, x0), c(y1, y1))  # bottom
  lines(c(x0, x0), c(y1, y0))  # left
}



# Three adjacent sheets: 42x9, 42x18, 42x5 stacked top→down
draw_three_sheets <- function(x0, y0) {
  draw_rectangle(x0, y0,     42,  9)   # 42×9
  draw_rectangle(x0, y0 - 9, 42, 18)   # 42×18 just below
  draw_rectangle(x0, y0 - 27,42,  5)   # 42×5  just below
}

draw_support <- function(x, y)
  draw_narrowing_rectangle(x, y, dial_opening_depth, 40)

open.pdf <- function(title, width_in_mm, height_in_mm, margin_in_mm) {
  pdf(file=title, width=(width_in_mm + 2 * margin_in_mm) / 25.4, height=(height_in_mm + 2 * margin_in_mm) / 25.4 )   #units: inches
  par(mai=c(margin_in_mm / 25.4, margin_in_mm / 25.4, margin_in_mm / 25.4, margin_in_mm / 25.4))  #mai - margins in inches
  plot.new()
  plot.window(c(0, width_in_mm), c(0, height_in_mm), asp=1, xaxs="i", yaxs="i")  #="i" to avoid scale by 4%
}

close.pdf <- function() {
  dev.off()
}




open.pdf("design_PDFs/base_both_sides.pdf", case_depth_in_feathers*feather_width_mm, base_height_in_feathers*feather_width_mm, 5)
draw_side_wall(case_depth_in_feathers, base_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/base_front_or_back.pdf", case_width_in_feathers*feather_width_mm, base_height_in_feathers*feather_width_mm, 10)
draw_side_wall(case_width_in_feathers, base_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/base_inner_back.pdf", case_width_in_feathers*feather_width_mm, base_height_in_feathers*feather_width_mm, 10)
draw_inner_back(case_width_in_feathers, base_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/base_bottom.pdf", case_width_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_top_or_bottom(case_width_in_feathers, case_depth_in_feathers)
close.pdf()

open.pdf("design_PDFs/lid_both_sides.pdf", case_depth_in_feathers*feather_width_mm, lid_height_in_feathers*feather_width_mm, 10)
draw_side_wall(case_depth_in_feathers, lid_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/lid_front_or_back.pdf", case_width_in_feathers*feather_width_mm, lid_height_in_feathers*feather_width_mm, 10)
draw_side_wall(case_width_in_feathers, lid_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/lid_inner_back.pdf", case_width_in_feathers*feather_width_mm, lid_height_in_feathers*feather_width_mm, 10)
draw_inner_back(case_width_in_feathers, lid_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/lid_top.pdf", case_width_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_top_or_bottom(case_width_in_feathers, case_depth_in_feathers, dial_opening_width, dial_opening_depth, opening_shift_mm=c(30, 21))
close.pdf()

open.pdf("design_PDFs/dial1.pdf", r_mm*2+10, r_mm*2+10, 10)
draw_dial(r_mm+5, r_mm+5, r_mm, pixel_vector = unlist(lapply(digits, function(m) m[,1])))
close.pdf()

open.pdf("design_PDFs/dial2.pdf", r_mm*2+10, r_mm*2+10, 10)
draw_dial(r_mm+5, r_mm+5, r_mm, pixel_vector = unlist(lapply(digits, function(m) m[,2])))
close.pdf()

open.pdf("design_PDFs/dial3.pdf", r_mm*2+10, r_mm*2+10, 10)
draw_dial(r_mm+5, r_mm+5, r_mm, pixel_vector = unlist(lapply(digits, function(m) m[,3])))
close.pdf()

open.pdf("design_PDFs/dial_control.pdf", r_mm*2+10, r_mm*2+10, 10)
draw_dial(r_mm+5, r_mm+5, r_mm)
close.pdf()

open.pdf("design_PDFs/small_parts.pdf", 100, 110, 10)
for (i in 1:6)
  draw_loop_lines_exact(7+i*8,12)

for (i in 1:6)
  draw_L(i*7,21)

draw_three_sheets(3,53)

draw_three_separators(3, 50)
draw_support(0, 110)

close.pdf()
