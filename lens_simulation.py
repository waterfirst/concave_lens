import streamlit as st
import numpy as np
import plotly.graph_objects as go


# 메니스커스 형태를 계산하는 함수
def calculate_meniscus(x, contact_angle, fill_height, width):
    angle_rad = contact_angle * np.pi / 180
    center = width / 2

    R = abs(center / np.cos(angle_rad))

    if contact_angle < 90:
        y = -(fill_height - (R - np.sqrt(np.maximum(0, R**2 - (x - center) ** 2))))
    elif contact_angle > 90:
        y = -(
            fill_height
            + (R - np.sqrt(np.maximum(0, R**2 - (x - center) ** 2)))
            - 2 * (R - fill_height)
        )
    else:
        y = np.full_like(x, -fill_height)

    y = np.maximum(-fill_height, np.minimum(y, 0))
    return y


# 스넬의 법칙을 사용한 굴절각 계산 함수
def calculate_refraction(angle_in, n1, n2):
    sin_out = (n1 / n2) * np.sin(angle_in)
    if abs(sin_out) > 1:
        return None  # 전반사
    return np.arcsin(sin_out)


# Streamlit 앱 시작
st.title("Concave Lens Simulation")

# 사이드바에 입력 위젯 배치
st.sidebar.header("파라미터 설정")
contact_angle = st.sidebar.slider("접촉각 (도)", min_value=10, max_value=170, value=27)
fill_height = st.sidebar.slider(
    "유체 높이 (μm)", min_value=0.1, max_value=10.0, value=7.5, step=0.1
)
n1 = st.sidebar.number_input(
    "하부막 굴절률", min_value=1.0, max_value=2.0, value=1.5, step=0.01
)
n2 = st.sidebar.number_input(
    "상부막 굴절률", min_value=1.0, max_value=2.0, value=1.7, step=0.01
)

# 시뮬레이션 로직
width = 20
height = 10

x = np.linspace(0, width, 400)
y = calculate_meniscus(x, contact_angle, fill_height, width)

# 광선 시뮬레이션
n_rays = 21
ray_x_start = np.linspace(0, width, n_rays)
ray_y_start = np.full(n_rays, -10)

ray_x_end = np.zeros(n_rays)
ray_y_end = np.zeros(n_rays)
refracted_x = np.zeros(n_rays)
refracted_y = np.zeros(n_rays)

for i in range(n_rays):
    # 메니스커스와의 교차점 찾기
    intersection = np.interp(ray_x_start[i], x, y)
    ray_x_end[i] = ray_x_start[i]
    ray_y_end[i] = intersection

    # 법선 벡터 계산
    idx = np.argmin(np.abs(x - ray_x_start[i]))
    if idx > 0 and idx < len(x) - 1:
        dx = x[idx + 1] - x[idx - 1]
        dy = y[idx + 1] - y[idx - 1]
        normal_angle = np.arctan2(dy, dx) + np.pi / 2  # 법선 각도

        # 입사각 계산
        incident_angle = abs(normal_angle - np.pi / 2)

        # 굴절각 계산
        refracted_angle = calculate_refraction(incident_angle, n1, n2)

        if refracted_angle is not None:
            # 굴절된 광선
            ray_angle = normal_angle - refracted_angle
            refracted_x[i] = ray_x_end[i] + np.cos(ray_angle) * height
            refracted_y[i] = ray_y_end[i] + np.sin(ray_angle) * height
        else:
            # 전반사
            refracted_x[i] = ray_x_end[i]
            refracted_y[i] = ray_y_end[i]
    else:
        # 경계 케이스 처리
        refracted_x[i] = ray_x_end[i]
        refracted_y[i] = 0

# x > 10인 경우의 굴절광만 선택하고 대칭 처리
valid_rays = ray_x_end > 10
ray_x_end_valid = ray_x_end[valid_rays]
ray_y_end_valid = ray_y_end[valid_rays]
refracted_x_valid = refracted_x[valid_rays]
refracted_y_valid = refracted_y[valid_rays]

# 대칭 처리
ray_x_end_symmetric = np.concatenate([ray_x_end_valid, 20 - ray_x_end_valid[::-1]])
ray_y_end_symmetric = np.concatenate([ray_y_end_valid, ray_y_end_valid[::-1]])
refracted_x_symmetric = np.concatenate(
    [refracted_x_valid, 20 - refracted_x_valid[::-1]]
)
refracted_y_symmetric = np.concatenate([refracted_y_valid, refracted_y_valid[::-1]])

# Plotly를 사용한 그래프 생성
fig = go.Figure()

# 외부 구조
fig.add_shape(
    type="rect",
    x0=0,
    y0=-10,
    x1=width,
    y1=0,
    line=dict(color="black", width=1),
    fillcolor="rgba(255,255,255,0)",
)

# 유체
fig.add_trace(
    go.Scatter(
        x=x,
        y=y,
        fill="tozeroy",
        fillcolor="rgba(173,216,230,0.5)",
        line=dict(color="rgba(173,216,230,0.5)"),
        showlegend=False,
    )
)

# 메니스커스 선
fig.add_trace(
    go.Scatter(
        x=x, y=y, mode="lines", line=dict(color="blue", width=2), name="Meniscus"
    )
)

# 입사 광선 (모든 광선 표시)
for i in range(n_rays):
    fig.add_trace(
        go.Scatter(
            x=[ray_x_start[i], ray_x_end[i]],
            y=[ray_y_start[i], ray_y_end[i]],
            mode="lines",
            line=dict(color="red"),
            showlegend=False,
        )
    )

# 굴절된 광선 (대칭 처리된 광선만 표시)
for i in range(len(ray_x_end_symmetric)):
    fig.add_trace(
        go.Scatter(
            x=[ray_x_end_symmetric[i], refracted_x_symmetric[i]],
            y=[ray_y_end_symmetric[i], refracted_y_symmetric[i]],
            mode="lines",
            line=dict(color="green"),
            showlegend=False,
        )
    )

fig.update_layout(
    title="음각렌즈 ray 시뮬레이션",
    xaxis_title="너비 (μm)",
    yaxis_title="높이 (μm)",
    xaxis=dict(range=[0, width], zeroline=False),
    yaxis=dict(range=[-10, 5], zeroline=False),
    width=800,
    height=600,
    showlegend=False,
)

# 그래프 표시
st.plotly_chart(fig)

# 접촉각 표시
st.write(f"메니스커스 접촉각: {contact_angle:.2f} 도")
